#include <kvs/CommandLine>
#include <kvs/Directory>
#include <kvs/File>
#include <kvs/FileList>
#include <kvs/String>
#include <kvs/Timer>

#include <algorithm>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <limits>
#include <optional>
#include <queue>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace fs = std::filesystem;

namespace
{

constexpr float Epsilon = 1.0e-12f;
constexpr std::size_t InvalidNode = std::numeric_limits<std::size_t>::max();

struct Config
{
    fs::path base_dir = ".";
    fs::path input_csv = "Output/output_video_params.csv";
    fs::path output_dir = "Output";
    fs::path export_dir = "ex_Output";
    int candidate_num = 0;
    int first_file = 0;
    float entropy_ratio = 1.0f;
    float focus_path_ratio = 1.0f;
    float camera_path_ratio = 1.0f;
    bool verbose = false;
};

struct Row
{
    std::string filename;
    float entropy = 0.0f;
    float entropy_cost = 0.0f;
    std::vector<float> focus_path;
    std::vector<float> camera_path;
};

struct Edge
{
    std::size_t to = 0;
    float weight = 0.0f;
};

struct ImageKey
{
    bool valid = false;
    int time = 0;
    int candidate = 0;
    int zoom_level = 0;
    int route = 0;
};

std::string trim( const std::string& text )
{
    const auto first = text.find_first_not_of( " \t\r\n" );
    if ( first == std::string::npos ) { return ""; }

    const auto last = text.find_last_not_of( " \t\r\n" );
    return text.substr( first, last - first + 1 );
}

std::vector<std::string> split( const std::string& text, const char delimiter )
{
    std::vector<std::string> values;
    std::istringstream stream( text );
    std::string value;
    while ( std::getline( stream, value, delimiter ) )
    {
        values.push_back( trim( value ) );
    }
    return values;
}

bool starts_with( const std::string& text, const std::string& prefix )
{
    return text.size() >= prefix.size() &&
        std::equal( prefix.begin(), prefix.end(), text.begin() );
}

float to_float( const std::string& text )
{
    const auto value = trim( text );
    return value.empty() ? 0.0f : kvs::String::To<float>( value );
}

int to_int( const std::string& text )
{
    const auto value = trim( text );
    return value.empty() ? 0 : kvs::String::To<int>( value );
}

float positive_cost( const float value )
{
    return std::max( value, Epsilon );
}

float normalized( const float value, const float sum )
{
    return sum > Epsilon ? value / sum : 0.0f;
}

void ensure_directory( const fs::path& path )
{
    const auto pathname = path.string();
    if ( kvs::Directory::Exists( pathname ) ) { return; }
    if ( kvs::Directory::Make( pathname ) ) { return; }

    fs::create_directories( path );
}

bool copy_file_overwrite( const fs::path& source, const fs::path& destination )
{
    if ( !kvs::File::Exists( source.string() ) )
    {
        std::cerr << "Skip missing file: " << source << std::endl;
        return false;
    }

    ensure_directory( destination.parent_path() );
    fs::copy_file( source, destination, fs::copy_options::overwrite_existing );
    return true;
}

int infer_candidate_num( const std::vector<std::string>& header )
{
    int count = 0;
    for ( std::size_t i = 2; i < header.size(); ++i )
    {
        if ( starts_with( header[i], "preFocusPath" ) ) { ++count; }
        else { break; }
    }
    return count;
}

std::vector<Row> read_rows( const fs::path& csv_path, int& candidate_num )
{
    if ( !kvs::File::Exists( csv_path.string() ) )
    {
        throw std::runtime_error( "CSV file does not exist: " + csv_path.string() );
    }

    std::ifstream file( csv_path );
    if ( !file )
    {
        throw std::runtime_error( "Cannot open CSV file: " + csv_path.string() );
    }

    std::string line;
    if ( !std::getline( file, line ) )
    {
        throw std::runtime_error( "CSV file is empty: " + csv_path.string() );
    }

    const auto header = split( line, ',' );
    if ( candidate_num <= 0 )
    {
        candidate_num = infer_candidate_num( header );
    }

    if ( candidate_num <= 0 )
    {
        throw std::runtime_error( "candidate-num must be specified or inferred from preFocusPath columns." );
    }

    const std::size_t focus_offset = 2;
    const std::size_t camera_offset = focus_offset + static_cast<std::size_t>( candidate_num );

    std::vector<Row> rows;
    while ( std::getline( file, line ) )
    {
        if ( trim( line ).empty() ) { continue; }

        const auto cells = split( line, ',' );
        if ( cells.size() < 2 )
        {
            throw std::runtime_error( "CSV row has fewer than two columns: " + line );
        }

        Row row;
        row.filename = cells[0];
        row.entropy = to_float( cells[1] );
        row.entropy_cost = 1.0f / positive_cost( row.entropy );
        row.focus_path.assign( candidate_num, 0.0f );
        row.camera_path.assign( candidate_num, 0.0f );

        for ( int i = 0; i < candidate_num; ++i )
        {
            const auto index = static_cast<std::size_t>( i );
            if ( focus_offset + index < cells.size() )
            {
                row.focus_path[index] = std::max( to_float( cells[focus_offset + index] ), 0.0f );
            }

            if ( camera_offset + index < cells.size() )
            {
                row.camera_path[index] = std::max( to_float( cells[camera_offset + index] ), 0.0f );
            }
        }

        rows.push_back( std::move( row ) );
    }

    return rows;
}

std::vector<std::vector<Edge>> build_graph(
    const std::vector<Row>& rows,
    const std::size_t used_rows,
    const int candidate_num,
    const Config& config )
{
    std::vector<std::vector<Edge>> graph( used_rows );
    const auto n = static_cast<std::size_t>( candidate_num );
    const auto nsteps = used_rows / n;

    for ( std::size_t step = 0; step + 1 < nsteps; ++step )
    {
        const auto from_base = step * n;
        const auto to_base = ( step + 1 ) * n;

        for ( std::size_t from = 0; from < n; ++from )
        {
            float sum_entropy = 0.0f;
            float sum_focus = 0.0f;
            float sum_camera = 0.0f;

            for ( std::size_t to = 0; to < n; ++to )
            {
                const auto to_node = to_base + to;
                sum_entropy += rows[to_node].entropy_cost;
                sum_focus += rows[to_node].focus_path[from];
                sum_camera += rows[to_node].camera_path[from];
            }

            const auto from_node = from_base + from;
            for ( std::size_t to = 0; to < n; ++to )
            {
                const auto to_node = to_base + to;
                const auto entropy_cost = normalized( rows[to_node].entropy_cost, sum_entropy );
                const auto focus_cost = normalized( rows[to_node].focus_path[from], sum_focus );
                const auto camera_cost = normalized( rows[to_node].camera_path[from], sum_camera );
                const auto weight =
                    config.entropy_ratio * entropy_cost +
                    config.focus_path_ratio * focus_cost +
                    config.camera_path_ratio * camera_cost;

                graph[from_node].push_back( Edge{ to_node, weight } );
            }
        }
    }

    return graph;
}

std::vector<float> dijkstra(
    const std::vector<std::vector<Edge>>& graph,
    const std::size_t start,
    std::vector<std::size_t>& parent )
{
    using QueueItem = std::pair<float, std::size_t>;

    std::vector<float> distance( graph.size(), std::numeric_limits<float>::max() );
    parent.assign( graph.size(), InvalidNode );

    std::priority_queue<QueueItem, std::vector<QueueItem>, std::greater<QueueItem>> queue;
    distance[start] = 0.0f;
    queue.emplace( 0.0f, start );

    while ( !queue.empty() )
    {
        const auto [ current_distance, current ] = queue.top();
        queue.pop();

        if ( current_distance > distance[current] ) { continue; }

        for ( const auto& edge : graph[current] )
        {
            const auto next_distance = distance[current] + edge.weight;
            if ( next_distance < distance[edge.to] )
            {
                distance[edge.to] = next_distance;
                parent[edge.to] = current;
                queue.emplace( next_distance, edge.to );
            }
        }
    }

    return distance;
}

std::size_t find_best_goal(
    const std::vector<float>& distance,
    const std::size_t used_rows,
    const int candidate_num )
{
    const auto n = static_cast<std::size_t>( candidate_num );
    const auto last_base = used_rows - n;
    auto best_goal = InvalidNode;
    auto best_distance = std::numeric_limits<float>::max();

    for ( std::size_t i = 0; i < n; ++i )
    {
        const auto node = last_base + i;
        if ( distance[node] < best_distance )
        {
            best_distance = distance[node];
            best_goal = node;
        }
    }

    return best_goal;
}

std::vector<std::size_t> build_path(
    const std::vector<std::size_t>& parent,
    const std::size_t goal )
{
    std::vector<std::size_t> path;
    for ( auto node = goal; node != InvalidNode; node = parent[node] )
    {
        path.push_back( node );
    }

    std::reverse( path.begin(), path.end() );
    return path;
}

ImageKey parse_image_key( const std::string& filename )
{
    const auto stem = fs::path( filename ).stem().string();
    const auto tokens = split( stem, '_' );

    if ( tokens.size() < 5 || tokens[0] != "output" )
    {
        return ImageKey{};
    }

    ImageKey key;
    key.valid = true;
    key.time = to_int( tokens[1] );
    key.candidate = to_int( tokens[2] );
    key.zoom_level = to_int( tokens[3] );
    key.route = to_int( tokens[4] );
    return key;
}

bool is_route_image(
    const ImageKey& key,
    const int time_from,
    const int time_to,
    const int candidate_num,
    const std::size_t from_node,
    const std::size_t to_node )
{
    if ( !key.valid ) { return false; }

    const auto route =
        static_cast<int>( from_node % candidate_num ) * candidate_num +
        static_cast<int>( to_node % candidate_num );

    return time_from < key.time && key.time < time_to && key.route == route;
}

std::size_t copy_selected_images(
    const std::vector<Row>& rows,
    const std::vector<std::size_t>& path,
    const int candidate_num,
    const Config& config )
{
    const auto base_dir = fs::absolute( config.base_dir );
    const auto source_output_dir = base_dir / config.output_dir;
    const auto export_root = base_dir / config.export_dir;
    const auto export_output_dir = export_root / config.output_dir;

    ensure_directory( export_root );
    ensure_directory( export_output_dir );

    const kvs::Directory output_dir( source_output_dir.string() );
    if ( !output_dir.exists() )
    {
        throw std::runtime_error( "Output directory does not exist: " + source_output_dir.string() );
    }

    const kvs::FileList output_files = output_dir.fileList( true );
    std::size_t copied = 0;

    for ( std::size_t i = 0; i < path.size(); ++i )
    {
        const auto node = path[i];
        const auto selected_source = base_dir / rows[node].filename;
        const auto selected_destination = export_root / rows[node].filename;
        if ( copy_file_overwrite( selected_source, selected_destination ) ) { ++copied; }

        if ( i == 0 ) { continue; }

        const auto from_node = path[i - 1];
        const auto from_key = parse_image_key( rows[from_node].filename );
        const auto to_key = parse_image_key( rows[node].filename );
        if ( !from_key.valid || !to_key.valid ) { continue; }

        for ( const auto& file : output_files )
        {
            if ( !file.isFile() ) { continue; }

            const auto key = parse_image_key( file.fileName() );
            if ( !is_route_image( key, from_key.time, to_key.time, candidate_num, from_node, node ) )
            {
                continue;
            }

            if ( copy_file_overwrite( file.filePath(), export_output_dir / file.fileName() ) )
            {
                ++copied;
            }
        }
    }

    return copied;
}

Config parse_legacy_command_line( const int argc, char** argv )
{
    Config config;
    config.candidate_num = kvs::String::To<int>( argv[1] );
    config.first_file = kvs::String::To<int>( argv[2] );
    config.entropy_ratio = kvs::String::To<float>( argv[3] );
    config.focus_path_ratio = kvs::String::To<float>( argv[4] );
    config.camera_path_ratio = kvs::String::To<float>( argv[5] );
    return config;
}

std::optional<Config> parse_command_line( const int argc, char** argv )
{
    if ( argc >= 6 && argv[1][0] != '-' )
    {
        return parse_legacy_command_line( argc, argv );
    }

    Config config;
    kvs::CommandLine command_line( argc, argv );
    command_line.addHelpOption();
    command_line.addOption( "base-dir", "target application directory", 1, false );
    command_line.addOption( "input-csv", "CSV path relative to base-dir", 1, false );
    command_line.addOption( "output-dir", "output image directory relative to base-dir", 1, false );
    command_line.addOption( "export-dir", "export directory relative to base-dir", 1, false );
    command_line.addOption( "candidate-num", "number of candidates per timestep", 1, false );
    command_line.addOption( "first-file", "start node index", 1, false );
    command_line.addOption( "entropy-ratio", "entropy cost ratio", 1, false );
    command_line.addOption( "focus-path-ratio", "focus path length cost ratio", 1, false );
    command_line.addOption( "camera-path-ratio", "camera path length cost ratio", 1, false );
    command_line.addOption( "verbose", "print selected path nodes", 0, false );

    for ( int i = 1; i < argc; ++i )
    {
        if ( std::string( argv[i] ) == "-h" )
        {
            command_line.showHelpMessage( kvs::CommandLine::UsageAndOption );
            return std::nullopt;
        }
    }

    if ( !command_line.parse() )
    {
        throw std::runtime_error( "Invalid command line." );
    }

    if ( command_line.hasOption( "base-dir" ) )
    {
        config.base_dir = command_line.optionValue<std::string>( "base-dir" );
    }
    if ( command_line.hasOption( "input-csv" ) )
    {
        config.input_csv = command_line.optionValue<std::string>( "input-csv" );
    }
    if ( command_line.hasOption( "output-dir" ) )
    {
        config.output_dir = command_line.optionValue<std::string>( "output-dir" );
    }
    if ( command_line.hasOption( "export-dir" ) )
    {
        config.export_dir = command_line.optionValue<std::string>( "export-dir" );
    }
    if ( command_line.hasOption( "candidate-num" ) )
    {
        config.candidate_num = command_line.optionValue<int>( "candidate-num" );
    }
    if ( command_line.hasOption( "first-file" ) )
    {
        config.first_file = command_line.optionValue<int>( "first-file" );
    }
    if ( command_line.hasOption( "entropy-ratio" ) )
    {
        config.entropy_ratio = command_line.optionValue<float>( "entropy-ratio" );
    }
    if ( command_line.hasOption( "focus-path-ratio" ) )
    {
        config.focus_path_ratio = command_line.optionValue<float>( "focus-path-ratio" );
    }
    if ( command_line.hasOption( "camera-path-ratio" ) )
    {
        config.camera_path_ratio = command_line.optionValue<float>( "camera-path-ratio" );
    }
    config.verbose = command_line.hasOption( "verbose" );

    return config;
}

void run( const Config& config )
{
    auto candidate_num = config.candidate_num;
    const auto csv_path = fs::absolute( config.base_dir / config.input_csv );
    auto rows = read_rows( csv_path, candidate_num );

    if ( rows.empty() )
    {
        throw std::runtime_error( "CSV contains no data rows." );
    }

    const auto n = static_cast<std::size_t>( candidate_num );
    const auto used_rows = rows.size() / n * n;
    if ( used_rows < n )
    {
        throw std::runtime_error( "CSV rows are fewer than candidate-num." );
    }
    if ( used_rows != rows.size() )
    {
        std::cerr << "Ignore trailing rows: " << rows.size() - used_rows << std::endl;
    }

    const auto start = static_cast<std::size_t>( config.first_file );
    if ( start >= used_rows )
    {
        throw std::runtime_error( "first-file is out of range." );
    }

    const auto graph = build_graph( rows, used_rows, candidate_num, config );

    kvs::Timer timer;
    std::vector<std::size_t> parent;
    timer.start();
    const auto distance = dijkstra( graph, start, parent );
    timer.stop();

    const auto best_goal = find_best_goal( distance, used_rows, candidate_num );
    if ( best_goal == InvalidNode )
    {
        throw std::runtime_error( "No path to the last timestep was found." );
    }

    const auto path = build_path( parent, best_goal );

    kvs::Timer copy_timer;
    copy_timer.start();
    const auto copied = copy_selected_images( rows, path, candidate_num, config );
    copy_timer.stop();

    std::cout << "candidate_num: " << candidate_num << std::endl;
    std::cout << "nodes: " << used_rows << std::endl;
    std::cout << "steps: " << used_rows / n << std::endl;
    std::cout << "start: " << start << std::endl;
    std::cout << "goal: " << best_goal << std::endl;
    std::cout << "distance: " << distance[best_goal] << std::endl;
    std::cout << "dijkstra: " << timer.msec() << " [ms]" << std::endl;
    std::cout << "copy: " << copy_timer.msec() << " [ms]" << std::endl;
    std::cout << "copied_files: " << copied << std::endl;

    if ( config.verbose )
    {
        std::cout << "path:";
        for ( const auto node : path ) { std::cout << ' ' << node; }
        std::cout << std::endl;
    }
}

} // namespace

int main( int argc, char** argv )
{
    try
    {
        const auto config = parse_command_line( argc, argv );
        if ( !config ) { return 0; }
        run( *config );
    }
    catch ( const std::exception& exception )
    {
        std::cerr << "CameraOptimalPathSearch error: " << exception.what() << std::endl;
        return 1;
    }

    return 0;
}
