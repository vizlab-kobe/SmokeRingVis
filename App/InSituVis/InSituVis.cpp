#if defined( KVS_SUPPORT_MPI )
#undef KVS_SUPPORT_MPI
#endif
#include <InSituVis/Lib/Adaptor.h>
#include <InSituVis/Lib/Viewpoint.h>
#include <kvs/StructuredVolumeObject>
#include <kvs/PolygonRenderer>
#include <kvs/RayCastingRenderer>
#include <kvs/Isosurface>
#include <kvs/OrthoSlice>
#include <kvs/Bounds>
#include <InSituVis/Lib/PolyhedralViewpoint.h>
#include <kvs/StochasticLineRenderer>
#include <kvs/StochasticPolygonRenderer>
#include <kvs/ParticleBasedRenderer>
#include <kvs/CellByCellMetropolisSampling>

// Parameters
namespace Params
{
const auto ImageSize = kvs::Vec2ui{ 512, 512 }; // width x height
const auto AnalysisInterval = 100; // analysis (visuaization) time interval
const auto ViewDir = InSituVis::Viewpoint::Direction::Uni; // Uni or Omni
const auto ViewDim = kvs::Vec3ui{ 1, 8, 2 }; // viewpoint dimension
// const auto Viewpoint = InSituVis::PolyhedralViewpoint{ ViewDim, ViewDir  }; // viewpoint

const auto m_min_coord = kvs::Vec3{ -4.5f, -4.5f, -4.5f };
const auto m_max_coord = kvs::Vec3{  4.5f,  4.5f,  4.5f };
const auto Viewpoint = InSituVis::PolyhedralViewpoint{ ViewDim, m_min_coord, m_max_coord,ViewDir };

const auto Repeats = 100; // number of repetitions for stochastic rendering
} // end of namespace Params

// Type definition
using Adaptor = InSituVis::Adaptor;
using Pipeline = Adaptor::Pipeline;
using Screen = Adaptor::Screen;
using Volume = kvs::StructuredVolumeObject;
using Object = kvs::ObjectBase;

// Visualization pipelines
inline Pipeline OrthoSlice( const kvs::ColorMap& cmap )
{
    return [cmap] ( Screen& screen, const Object& object, const std::string& base_dir, int time_step )
    {
        Volume volume; volume.shallowCopy( Volume::DownCast( object ) );

        // Setup a transfer function.
        const auto min_value = volume.minValue();
        const auto max_value = volume.maxValue();
        auto t = kvs::TransferFunction( cmap );
        t.setRange( min_value, max_value );

        // Create new slice objects.
        auto p0 = ( volume.minObjectCoord().y() + volume.maxObjectCoord().y() ) * 0.5f;
        auto a0 = kvs::OrthoSlice::YAxis;
        auto* o0 = new kvs::OrthoSlice( &volume, p0, a0, t );
        o0->setName( "Slice0" );

        auto p1 = ( volume.minObjectCoord().z() + volume.maxObjectCoord().z() ) * 0.5f;
        auto a1 = kvs::OrthoSlice::ZAxis;
        auto* o1 = new kvs::OrthoSlice( &volume, p1, a1, t );
        o1->setName( "Slice1" );

        // Register object and renderer to screen
        kvs::Light::SetModelTwoSide( true );
        if ( screen.scene()->hasObject( "Slice0" ) )
        {
            // Update the objects.
            screen.scene()->replaceObject( "Slice0", o0 );
            screen.scene()->replaceObject( "Slice1", o1 );
        }
        else
        {
            // Bounding box.
            screen.registerObject( o0, new kvs::Bounds() );

            // Register the objects with renderer.
            auto* r0 = new kvs::glsl::PolygonRenderer();
            auto* r1 = new kvs::glsl::PolygonRenderer();
            r0->setTwoSideLightingEnabled( true );
            r1->setTwoSideLightingEnabled( true );
            screen.registerObject( o0, r0 );
            screen.registerObject( o1, r1 );
        }
    };
};

inline Pipeline Isosurface( const kvs::ColorMap& cmap )
{
    return [cmap] ( Screen& screen, const Object& object, const std::string& base_dir, int time_step )
    {
        Volume volume; volume.shallowCopy( Volume::DownCast( object ) );

        // Setup a transfer function.
        const auto min_value = volume.minValue();
        const auto max_value = volume.maxValue();
        auto t = kvs::TransferFunction( cmap );
        t.setRange( min_value, max_value );

        // Create new object
        auto n = kvs::Isosurface::VertexNormal;
        auto d = true;

        auto i0 = kvs::Math::Mix( min_value, max_value, 0.1 );
        auto i1 = kvs::Math::Mix( min_value, max_value, 0.6 );
        auto i2 = kvs::Math::Mix( min_value, max_value, 0.7 );
        auto i3 = kvs::Math::Mix( min_value, max_value, 0.9 );
        auto* o0 = new kvs::Isosurface( &volume, i0, n, d, t );
        auto* o1 = new kvs::Isosurface( &volume, i1, n, d, t );
        auto* o2 = new kvs::Isosurface( &volume, i2, n, d, t );
        auto* o3 = new kvs::Isosurface( &volume, i3, n, d, t );
        o0->setName( "Isosurface0" );
        o1->setName( "Isosurface1" );
        o2->setName( "Isosurface2" );
        o3->setName( "Isosurface3" );

        // Register object and renderer to screen
        kvs::Light::SetModelTwoSide( true );
        if ( screen.scene()->hasObject( "Isosurface0" ) )
        {
            // Update the objects.
            screen.scene()->replaceObject( "Isosurface0", o0 );
            screen.scene()->replaceObject( "Isosurface1", o1 );
            screen.scene()->replaceObject( "Isosurface2", o2 );
            screen.scene()->replaceObject( "Isosurface3", o3 );
        }
        else
        {
            // Bounding box.
            screen.registerObject( o0, new kvs::Bounds() );

            // Register the objects with renderer.
            auto* r0 = new kvs::glsl::PolygonRenderer();
            auto* r1 = new kvs::glsl::PolygonRenderer();
            auto* r2 = new kvs::glsl::PolygonRenderer();
            auto* r3 = new kvs::glsl::PolygonRenderer();
            r0->setTwoSideLightingEnabled( true );
            r1->setTwoSideLightingEnabled( true );
            r2->setTwoSideLightingEnabled( true );
            r3->setTwoSideLightingEnabled( true );
            screen.registerObject( o0, r0 );
            screen.registerObject( o1, r1 );
            screen.registerObject( o2, r2 );
            screen.registerObject( o3, r3 );
        }
    };
};

inline Pipeline VolumeRendering( const kvs::ColorMap& cmap )
{
    return [cmap] ( Screen& screen, const Object& object, const std::string& base_dir, int time_step)
    {
        auto* o = new Volume();
        o->shallowCopy( Volume::DownCast( object ) );
        o->setName( "Volume" );

        // Register object and renderer to screen
        if ( screen.scene()->hasObject( "Volume" ) )
        {
            // Update the objects.
            screen.scene()->replaceObject( "Volume", o );
        }
        else
        {
            // Bounding box.
            //screen.registerObject( o, new kvs::Bounds() );

            // Setup a transfer function.
            auto omap = kvs::OpacityMap();
            omap.addPoint(   0.0, 0.0 );
            omap.addPoint(   1.0, 0.2 );
            omap.addPoint( 250.0, 0.5 );
            omap.addPoint( 253.0, 0.1 );
            omap.addPoint( 255.0, 0.2 );
            omap.create();

            const auto min_value = o->minValue();
            const auto max_value = o->maxValue();
            auto tfunc = kvs::TransferFunction( cmap, omap );
            tfunc.setRange( min_value, max_value );

            // Register the objects with renderer.
            //auto* r = new kvs::glsl::RayCastingRenderer();
            auto* r = new kvs::RayCastingRenderer();
            r->setTransferFunction( tfunc );
            screen.registerObject( o, r );

            // ===== ここから: Stochastic サンプリングを一時登録してダンプ（execRendering と同条件） =====

            // 1) mesh の外形を volume(o) にも point にも反映できるよう mesh を取得
            const auto* mesh = kvs::PolygonObject::DownCast( screen.scene()->object( "BoundaryMesh" ) );
            if ( mesh )
            {
                const auto min_coord = mesh->minExternalCoord();
                const auto max_coord = mesh->maxExternalCoord();
                o->setMinMaxExternalCoords( min_coord, max_coord ); // レイキャストの座標基準合わせ（任意）
            }

            // 2) サンプリングして PointObject を取得
            using Sampler = kvs::CellByCellMetropolisSampling;
            const auto* camera_for_sampling = screen.scene()->camera();
            const size_t repeats_sampling = 1; // ← ここで N 回（「1回だけ」なら 1）
            const float  step_sampling    = 0.5f / 1000.0f;
            auto* point = new Sampler( camera_for_sampling, o, repeats_sampling, step_sampling, tfunc );

            // 3) PointObject に範囲と名前を付与（execRendering と合わせる）
            const std::string point_name = std::string("VolumeSamplingPoints");
            point->setName( point_name );
            if ( mesh )
            {
                const auto min_coord = mesh->minExternalCoord();
                const auto max_coord = mesh->maxExternalCoord();
                point->setMinMaxObjectCoords(  min_coord, max_coord );
                point->setMinMaxExternalCoords( min_coord, max_coord );
            }

            // 4) 一旦シーンに登録（★登録後の個体を使ってダンプするため）
            auto* point_renderer = new kvs::glsl::ParticleBasedRenderer();
            point_renderer->setTwoSideLightingEnabled( true );
            point_renderer->setRepetitionLevel( repeats_sampling );

            // 表示に影響させたくないなら（完全非表示にしたいなら）
            // point_renderer->setEnabled( false ); // 実装により有無。なければ最後に remove する。
            if ( screen.scene()->hasObject( point_name ) )
            {
                screen.scene()->replaceObject( point_name, point );
            }
            else
            {
                screen.registerObject( point, point_renderer );
            }

            // 5) 登録“後”の個体を取り直し（execRendering と同条件で Object→World 変換）
            kvs::PointObject* reg_point = kvs::PointObject::DownCast( screen.scene()->object( point_name ) );
            if ( reg_point )
            {
                // 出力ディレクトリは execRendering に合わせると混乱がない
                const std::string params_dir = base_dir + "/points";
                {
                    struct stat st;
                    if ( ::stat( params_dir.c_str(), &st ) != 0 )
                    {
                        ::mkdir( params_dir.c_str(), 0755 );
                    }
                }

                const std::string step_str = kvs::String::From( time_step, 6, '0' );
                const std::string json_path = params_dir + "/points_" + step_str + "_" + point_name + ".json";

                // ダンプ（execRendering と同じく to_world=true で変換）
                const auto& coords  = reg_point->coords();
                const auto& colors  = reg_point->colors();
                const auto& normals = reg_point->normals();
                const size_t nv = reg_point->numberOfVertices();

                std::ofstream ofs( json_path );
                if ( ofs.is_open() )
                {
                    ofs << "{\n";
                    ofs << "  \"time_step\": " << time_step << ",\n";
                    ofs << "  \"object_name\": \"" << point_name << "\",\n";
                    ofs << "  \"num_points\": " << nv << ",\n";

                    ofs << "  \"coords\": [\n";
                    for ( size_t vi = 0; vi < nv; ++vi )
                    {
                        const kvs::Vec3 p_obj( coords[3*vi+0], coords[3*vi+1], coords[3*vi+2] );
                        const kvs::Vec3 p = kvs::ObjectCoordinate( p_obj, reg_point ).toWorldCoordinate().position();
                        ofs << "    [" << p.x() << ", " << p.y() << ", " << p.z() << "]";
                        if ( vi + 1 < nv ) ofs << ",";
                        ofs << "\n";
                    }
                    ofs << "  ],\n";

                    ofs << "  \"colors\": [\n";
                    for ( size_t vi = 0; vi < nv; ++vi )
                    {
                        ofs << "    [" << int(colors[3*vi+0]) << ", " << int(colors[3*vi+1]) << ", " << int(colors[3*vi+2]) << "]";
                        if ( vi + 1 < nv ) ofs << ",";
                        ofs << "\n";
                    }
                    ofs << "  ],\n";

                    ofs << "  \"normals\": [\n";
                    if ( normals.size() >= 3*nv )
                    {
                        for ( size_t vi = 0; vi < nv; ++vi )
                        {
                            kvs::Vec3 n_obj( normals[3*vi+0], normals[3*vi+1], normals[3*vi+2] );
                            n_obj.normalize();
                            const kvs::Vec3 n = kvs::ObjectCoordinate( n_obj, reg_point ).toWorldCoordinate().position();
                            ofs << "    [" << n.x() << ", " << n.y() << ", " << n.z() << "]";
                            if ( vi + 1 < nv ) ofs << ",";
                            ofs << "\n";
                        }
                    }
                    ofs << "  ]\n";
                    ofs << "}\n";

                    std::cout << "Wrote point cloud to " << json_path
                            << " (#points=" << nv << ")\n";
                }
                else
                {
                    std::cerr << "Error: Cannot open " << json_path << " for writing.\n";
                }

                // 6) 可視化に影響させないなら登録解除（※API は環境により removeObject の形が異なる場合あり）
                //   - 名前指定が無ければ、オブジェクトポインタで remove する関数に合わせてください
                screen.scene()->removeObject( point_name ); // 例：名前で消せる実装の場合
                // もし removeObject(name) が無ければ:
                // screen.scene()->removeObject( reg_point );
            }
            // ===== ここまで: ダンプ用の一時登録ブロック ====
        }
    };
};

inline Pipeline StochasticRendering( const kvs::ColorMap& cmap ,const size_t repeats )
{
    return [cmap, repeats] ( Screen& screen, const Object& object, const std::string& base_dir, int time_step )
    {
        Volume volume; volume.shallowCopy( Volume::DownCast( object ) );
        if ( volume.numberOfCells() == 0 ) { return; }

        const auto* mesh = kvs::PolygonObject::DownCast( screen.scene()->object( "BoundaryMesh" ) );
        if ( mesh )
        {
            const auto min_coord = mesh->minExternalCoord();
            const auto max_coord = mesh->maxExternalCoord();
            volume.setMinMaxExternalCoords( min_coord, max_coord );
        }

        // Setup a transfer function.
        const auto min_value = volume.minValue();
        const auto max_value = volume.maxValue();

        //auto c = kvs::ColorMap::CoolWarm( 256 );
        // auto cmap = kvs::ColorMap::BrewerSpectral( 256 );
        auto omap = kvs::OpacityMap( 256 );
        omap.addPoint(   0, 0.0 );
        omap.addPoint(  30, 0.0 );
        omap.addPoint(  50, 0.2 );
        omap.addPoint( 100, 0.3 );
        omap.addPoint( 240, 0.2 );
        omap.addPoint( 245, 0.1 );
        omap.addPoint( 255, 0.0 );
        omap.create();
        auto t = kvs::TransferFunction( cmap, omap );
        //auto t = kvs::TransferFunction( c );
        t.setRange( min_value, max_value );

        // Particle generation.
        using Sampler = kvs::CellByCellMetropolisSampling;
        const auto* camera = screen.scene()->camera();
        const auto step = 0.5f / 1000.0f;
        auto* point = new Sampler( camera, &volume, repeats, step, t );

        // ▼ 追加: 出力ディレクトリ "base/params" を用意
        const std::string points_dir = base_dir + "/points";
        {
            struct stat st;
            if ( ::stat( points_dir.c_str(), &st ) != 0 )
            {
                if ( ::mkdir( points_dir.c_str(), 0755 ) != 0 )
                {
                    std::cerr << "Warning: Failed to create directory " << points_dir << "\n";
                }
            }
        }

        // データを取り出し -> これがしたい
        const auto& coords  = point->coords();   // float配列 (x,y,z)*N
        const auto& colors  = point->colors();   // uint8配列 (r,g,b)*N
        const auto& normals = point->normals();  // float配列 (nx,ny,nz)*N
        const size_t nv = point->numberOfVertices();
        const std::string step_str = kvs::String::From( time_step, 6, '0' );
        std::cout << "  \"num_points\": " << nv << ",\n";

        const std::string json_path =
            points_dir + "/points_" + step_str + "_sampling" + ".json";

        std::ofstream ofs( json_path );
        if ( !ofs.is_open() )
        {
            std::cerr << "Error: Cannot open " << json_path << " for writing.\n";
            return;
        }
        
        // Object→World 変換が必要なら true（体積や行列が入っている場合など）
        const bool to_world = true;

        ofs << "{\n";
        ofs << "  \"time_step\": " << time_step << ",\n";
        ofs << "  \"num_points\": " << nv << ",\n";

            // 座標
        ofs << "  \"coords\": [\n";
        for ( size_t vi = 0; vi < nv; ++vi )
        {
            const kvs::Vec3 p_obj( coords[3*vi+0], coords[3*vi+1], coords[3*vi+2] );
            kvs::Vec3 p = p_obj;
            if ( to_world )
            {
                p = kvs::ObjectCoordinate( p_obj, point ).toWorldCoordinate().position();
            }
            ofs << "    [" << p.x() << ", " << p.y() << ", " << p.z() << "]";
            if ( vi + 1 < nv ) ofs << ",";
            ofs << "\n";
        }
        ofs << "  ],\n";

        // 色
        ofs << "  \"colors\": [\n";
        for ( size_t vi = 0; vi < nv; ++vi )
        {
            ofs << "    ["
                << int(colors[3*vi+0]) << ", "
                << int(colors[3*vi+1]) << ", "
                << int(colors[3*vi+2]) << "]";
            if ( vi + 1 < nv ) ofs << ",";
            ofs << "\n";
        }
        ofs << "  ],\n";

        // 法線（あれば）
        ofs << "  \"normals\": [\n";
        if ( normals.size() >= 3*nv )
        {
            for ( size_t vi = 0; vi < nv; ++vi )
            {
                kvs::Vec3 n_obj( normals[3*vi+0], normals[3*vi+1], normals[3*vi+2] );
                n_obj.normalize();
                kvs::Vec3 n = n_obj;
                if ( to_world )
                {
                    n = kvs::ObjectCoordinate( n_obj, point ).toWorldCoordinate().position();
                }
                ofs << "    [" << n.x() << ", " << n.y() << ", " << n.z() << "]";
                if ( vi + 1 < nv ) ofs << ",";
                ofs << "\n";
            }
        }
        ofs << "  ]\n";
        ofs << "}\n";

        std::cout << "Wrote point cloud to " << json_path
                    << " (#points=" << nv << ")\n";

        point->setName( volume.name() + "Object");
        if ( mesh )
        {
            const auto min_coord = mesh->minExternalCoord();
            const auto max_coord = mesh->maxExternalCoord();
            point->setMinMaxObjectCoords( min_coord, max_coord );
            point->setMinMaxExternalCoords( min_coord, max_coord );
        }

        // Register object and renderer to screen
        //kvs::Light::SetModelTwoSide( true );
        if ( screen.scene()->hasObject( volume.name() + "Object") )
        {
            // Update the objects.
            screen.scene()->replaceObject( volume.name() + "Object", point );
        }
        else
        {
            // Register the objects with renderer.
            auto* point_renderer = new kvs::glsl::ParticleBasedRenderer();
            point_renderer->setTwoSideLightingEnabled( true );
            point_renderer->setRepetitionLevel( repeats );
            screen.registerObject( point, point_renderer );
        }
    };
}


extern "C"
{

Adaptor* InSituVis_new( const int method )
{
    auto vis = new Adaptor();

    // ▼▼ ここを追加：出力ベース/サブディレクトリを InSituVis.cpp から指定 ▼▼
    // 例）環境や用途に合わせてここだけ書き換えれば良い
    {
        std::string base_dir =  "/data2/tomoya/SmokeRing/Output";
        std::string sub_dir  = "Process";

        vis->outputDirectory().setBaseDirectoryName( base_dir );
        vis->outputDirectory().setSubDirectoryName( sub_dir );
    }

    vis->setImageSize( Params::ImageSize.x(), Params::ImageSize.y() );
    vis->setViewpoint( Params::Viewpoint );
    vis->setAnalysisInterval( Params::AnalysisInterval );

    const auto cmap = kvs::ColorMap::CoolWarm();
    switch ( method )
    {
    case 1: vis->setPipeline( OrthoSlice( cmap ) ); break;
    case 2: vis->setPipeline( Isosurface( cmap ) ); break;
    case 3: vis->setPipeline( VolumeRendering( cmap ) ); break;
    case 4: vis->setPipeline( StochasticRendering( cmap , Params::Repeats) ); break;
    default: break;
    }

    return vis;
}

void InSituVis_delete( Adaptor* self )
{
    if ( self ) delete self;
}

void InSituVis_initialize( Adaptor* self )
{
    self->initialize();
}

void InSituVis_finalize( Adaptor* self )
{
    self->finalize();
}

void InSituVis_put( Adaptor* self, double* values, int dimx, int dimy, int dimz )
{
    Volume volume;
    volume.setVeclen( 1 );
    volume.setResolution( kvs::Vec3ui( dimx, dimy, dimz ) );
    volume.setValues( kvs::ValueArray<double>{ values, size_t( dimx * dimy * dimz ) } );
    volume.setGridTypeToUniform();
    volume.updateMinMaxValues();
    volume.updateMinMaxCoords();

    self->put( volume );
}

void InSituVis_exec( Adaptor* self, double time_value, long time_index )
{
    self->exec( { float( time_value ), size_t( time_index ) } );
}

} // end of extern "C"


inline void SavePointToJson(
    const kvs::CellByCellMetropolisSampling* point,
    const std::string& filename,
    const bool to_world = false )
{
    if ( !point )
    {
        std::cerr << "[SavePointToJson] Error: null pointer.\n";
        return;
    }

    const auto& coords  = point->coords();   // float配列 (x,y,z)*N
    const auto& colors  = point->colors();   // uint8配列 (r,g,b)*N
    const auto& normals = point->normals();  // float配列 (nx,ny,nz)*N
    const size_t nv = coords.size() / 3;

    std::ofstream ofs( filename );
    if ( !ofs.is_open() )
    {
        std::cerr << "[SavePointToJson] Error: cannot open file " << filename << "\n";
        return;
    }

    ofs << std::fixed << std::setprecision(6);
    ofs << "{\n";

    // === coords ===
    ofs << "  \"coords\": [\n";
    for ( size_t vi = 0; vi < nv; ++vi )
    {
        kvs::Vec3 p_obj( coords[3*vi+0], coords[3*vi+1], coords[3*vi+2] );
        kvs::Vec3 p = p_obj;
        if ( to_world )
        {
            p = kvs::ObjectCoordinate( p_obj, point ).toWorldCoordinate().position();
        }
        ofs << "    [" << p.x() << ", " << p.y() << ", " << p.z() << "]";
        if ( vi + 1 < nv ) ofs << ",";
        ofs << "\n";
    }
    ofs << "  ],\n";

    // === colors ===
    ofs << "  \"colors\": [\n";
    for ( size_t vi = 0; vi < nv; ++vi )
    {
        ofs << "    [" 
            << static_cast<int>( colors[3*vi+0] ) << ", "
            << static_cast<int>( colors[3*vi+1] ) << ", "
            << static_cast<int>( colors[3*vi+2] ) << "]";
        if ( vi + 1 < nv ) ofs << ",";
        ofs << "\n";
    }
    ofs << "  ],\n";

    // === normals ===
    ofs << "  \"normals\": [\n";
    for ( size_t vi = 0; vi < nv; ++vi )
    {
        ofs << "    [" 
            << normals[3*vi+0] << ", "
            << normals[3*vi+1] << ", "
            << normals[3*vi+2] << "]";
        if ( vi + 1 < nv ) ofs << ",";
        ofs << "\n";
    }
    ofs << "  ]\n";

    ofs << "}\n";
    ofs.close();

    std::cout << "[SavePointToJson] Saved " << nv << " points to " << filename << std::endl;
}

