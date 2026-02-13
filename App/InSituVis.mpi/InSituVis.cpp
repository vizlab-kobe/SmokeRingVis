/*****************************************************************************/
/**
 *  @file   InSituVis.cpp
 *  @author Naohisa Sakamoto
 */
/*****************************************************************************/
#include <InSituVis/Lib/Adaptor_mpi.h>
#include <InSituVis/Lib/Viewpoint.h>
#include <kvs/StructuredVolumeObject>
#include <kvs/LineObject>
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


// Base adaptor
using AdaptorBase = InSituVis::mpi::Adaptor;

/*===========================================================================*/
/*
 * Parameters
 */
/*===========================================================================*/
namespace Params
{

// Flags for data output
struct Output
{
    static const auto Image = false;          // output rendering images
    static const auto SubImage = true;      // output sub-images for each process
    static const auto SubImageDepth = false; // output depth images for each sub-image
    static const auto SubImageAlpha = false; // output alpha images for each sub-image
};

// Basic parameters
const auto ImageSize = kvs::Vec2ui{ 512, 512 }; // width x height
const auto AnalysisInterval = 50; // analysis (visuaization) time interval

// Viewpoint settings
// const auto ViewPos = kvs::Vec3{ 7, 5, 6 }; // viewpoint position
const auto ViewDir = InSituVis::Viewpoint::Direction::Uni; // Uni or Omni
// const auto Viewpoint = InSituVis::Viewpoint{ { ViewDir, ViewPos } }; // viewpoint
// const auto ViewDim = kvs::Vec3ui{ 1, 8, 2 }; // viewpoint dimension
const auto m_min_coord = kvs::Vec3{ -3.0f, -3.0f, -3.0f };
const auto m_max_coord = kvs::Vec3{  3.0f,  3.0f,  3.0f };
const auto look_at_coord = kvs::Vec3{ -0.8, 0, 0};

const auto ViewDim1 = kvs::Vec3ui{ 1, 8, 1 }; // viewpoint dimension
const auto ViewDim2 = kvs::Vec3ui{ 1, 8, 2 }; // viewpoint dimension
const auto ViewDim3 = kvs::Vec3ui{ 1, 4, 2 }; // viewpoint dimension
const auto ViewDim4 = kvs::Vec3ui{ 1, 4, 3 }; // viewpoint dimension
const auto ViewDim5 = kvs::Vec3ui{ 1, 20, 1 }; // viewpoint dimension

const auto Viewpoint1 = InSituVis::PolyhedralViewpoint{ ViewDim1, m_min_coord, m_max_coord, look_at_coord, ViewDir };
const auto Viewpoint2 = InSituVis::PolyhedralViewpoint{ ViewDim2, m_min_coord, m_max_coord, look_at_coord, ViewDir };
const auto Viewpoint3 = InSituVis::PolyhedralViewpoint{ ViewDim3, m_min_coord, m_max_coord, look_at_coord, ViewDir };
const auto Viewpoint4 = InSituVis::PolyhedralViewpoint{ ViewDim4, m_min_coord, m_max_coord, look_at_coord, ViewDir };
const auto Viewpoint5 = InSituVis::PolyhedralViewpoint{ ViewDim5, m_min_coord, m_max_coord, look_at_coord, ViewDir };



} // end of namespace Params

/*===========================================================================*/
/**
 *  @brief  Adaptor class.
 */
/*===========================================================================*/
class Adaptor : public AdaptorBase
{
public:
    using BaseClass = AdaptorBase;
    using Pipeline = AdaptorBase::Pipeline;
    using Screen = AdaptorBase::Screen;
    using Volume = kvs::StructuredVolumeObject;
    using Object = kvs::ObjectBase;

private:
    kvs::Vec3ui m_global_dims{ 0, 0, 0 }; ///< resolution of whole volume
    kvs::Vec3ui m_offset{ 0, 0, 0 }; ///< offset to the sub-volume
    kvs::ColorMap m_cmap{ 256 }; ///< colormap

public:
    Adaptor() = default;
    virtual ~Adaptor() = default;

    const kvs::Vec3ui& globalDims() const { return m_global_dims; }
    const kvs::Vec3ui& offset() const { return m_offset; }
    const kvs::ColorMap& colorMap() const { return m_cmap; }

    void setGlobalDims( const kvs::Vec3ui& dims ) { m_global_dims = dims; }
    void setOffset( const kvs::Vec3ui& offs ) { m_offset = offs; }
    void setColorMap( const kvs::ColorMap& cmap ) { m_cmap = cmap; }

    void exec( const SimTime sim_time )
    {
        this->set_min_max_values();
        this->set_global_bounds();
        BaseClass::exec( sim_time );
    }

private:
    void set_min_max_values()
    {
        auto min_value = Volume::DownCast( BaseClass::objects().begin()->get() )->minValue();
        auto max_value = Volume::DownCast( BaseClass::objects().begin()->get() )->maxValue();
        for ( auto& object : BaseClass::objects() )
        {
            auto* volume = Volume::DownCast( object.get() );
            volume->updateMinMaxValues();

            min_value = kvs::Math::Min( min_value, volume->minValue() );
            max_value = kvs::Math::Max( max_value, volume->maxValue() );
        }

        BaseClass::world().allReduce( min_value, min_value, MPI_MIN );
        BaseClass::world().allReduce( max_value, max_value, MPI_MAX );

        for ( auto& object : BaseClass::objects() )
        {
            auto* volume = Volume::DownCast( object.get() );
            volume->setMinMaxValues( min_value, max_value );
        }
    }

    void set_global_bounds()
    {
        if ( !BaseClass::screen().scene()->hasObject( "Bounds" ) )
        {
            auto min_coord = kvs::Vec3{ 0, 0, 0 };
            auto max_coord = kvs::Vec3{ m_global_dims } - kvs::Vec3{ 1, 1, 1 };

            Object dummy;
            dummy.setMinMaxObjectCoords( min_coord, max_coord );
            dummy.setMinMaxExternalCoords( min_coord, max_coord );

            const bool visible = BaseClass::isAlphaBlendingEnabled() ? false : BaseClass::world().isRoot();
            kvs::Bounds bounds( kvs::RGBColor::Black(), 2.0f );
            auto* object = bounds.outputLineObject( &dummy );
            object->setName( "Bounds" );
            object->setVisible( false );
            BaseClass::screen().registerObject( object );
        }
    }

public:
    static Pipeline OrthoSlice( const Adaptor* adaptor )
    {
        return [adaptor] ( Screen& screen, const Object& object, const std::string& base_dir, int time_step)
        {
            Volume volume; volume.shallowCopy( Volume::DownCast( object ) );

            // Setup a transfer function.
            const auto min_value = volume.minValue();
            const auto max_value = volume.maxValue();
            auto t = kvs::TransferFunction( adaptor->colorMap() );
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

    static Pipeline Isosurface( const Adaptor* adaptor )
    {
        return [adaptor] ( Screen& screen, const Object& object,  const std::string& base_dir, int time_step)
        {
            Volume volume; volume.shallowCopy( Volume::DownCast( object ) );

            // Setup a transfer function.
            const auto min_value = volume.minValue();
            const auto max_value = volume.maxValue();
            auto t = kvs::TransferFunction( adaptor->colorMap() );
            t.setRange( min_value, max_value );

            // Create new object
            auto n = kvs::Isosurface::VertexNormal;
            auto d = true;

            auto i = kvs::Math::Mix( min_value, max_value, 0.1 );
            auto* o = new kvs::Isosurface( &volume, i, n, d, t );
            o->setName( "Isosurface" );

            // Register object and renderer to screen
            kvs::Light::SetModelTwoSide( true );
            if ( screen.scene()->hasObject( "Isosurface" ) )
            {
                // Update the objects.
                screen.scene()->replaceObject( "Isosurface", o );
            }
            else
            {
                // Register the objects with renderer.
                auto* r = new kvs::glsl::PolygonRenderer();
                r->setTwoSideLightingEnabled( true );
                screen.registerObject( o, r );
            }
        };
    };

    static Pipeline VolumeRendering( const Adaptor* adaptor )
    {
        return [adaptor] ( Screen& screen, const Object& object, const std::string& base_dir, int time_step)
        {
            auto* o = new Volume();
            o->shallowCopy( Volume::DownCast( object ) );
            o->setName( "Volume" );

            // ----- 生データ獲得のためのコード (単一 Volume 版) -----

            // ランクなどの情報
            const int rank = const_cast<Adaptor*>( adaptor )->world().rank();
            const std::string params_dir = base_dir + "/sample_volume";
            const std::string step_str   = kvs::String::From( time_step, 6, '0' );
            const std::string rank_str   = kvs::String::From( rank, 3, '0' );
            const std::string json_path  = params_dir + "/volume_" + step_str + "_rank" + rank_str + ".json";

            // ディレクトリ作成（存在しなければ作る）
            {
                struct stat st;
                if ( ::stat( params_dir.c_str(), &st ) != 0 )
                {
                    if ( ::mkdir( params_dir.c_str(), 0755 ) != 0 )
                    {
                        std::cerr << "Warning: Failed to create directory: " << params_dir << std::endl;
                    }
                    else
                    {
                        std::cout << "[Rank " << rank << "] Created directory: " << params_dir << std::endl;
                    }
                }
            }

            // ここで o は Volume* （= StructuredVolumeObject 互換）を想定
            const Volume& vol = *o;

            std::ofstream ofs( json_path );
            if ( ofs.is_open() )
            {
                ofs << "{\n";
                ofs << "  \"time_step\": " << time_step << ",\n";
                ofs << "  \"rank\": " << rank << ",\n";
                ofs << "  \"volume\": {\n";

                // 基本情報
                const auto res  = vol.resolution(); // Vec3ui (nx, ny, nz)
                const auto& vals = vol.values();    // AnyValueArray

                const size_t nx = static_cast<size_t>( res.x() );
                const size_t ny = static_cast<size_t>( res.y() );
                const size_t nz = static_cast<size_t>( res.z() );
                const size_t n_voxels = nx * ny * nz;

                ofs << "    \"name\": \"" << vol.name() << "\",\n";
                ofs << "    \"resolution\": [" << res.x() << ", "
                                            << res.y() << ", "
                                            << res.z() << "],\n";
                ofs << "    \"num_voxels\": " << n_voxels << ",\n";
                ofs << "    \"values_count\": " << vals.size() << ",\n";

                // ========= values の中身を全部書き出す =========
                ofs << "    \"values\": [\n";

                // ★ 実際の型に合わせてここだけ調整（典型的には kvs::Real32 か kvs::Real64）
                const auto arr = vals.asValueArray<kvs::Real32>();

                for ( size_t i = 0; i < arr.size(); ++i )
                {
                    ofs << "      " << static_cast<double>( arr[i] );
                    if ( i + 1 < arr.size() ) ofs << ",";
                    ofs << "\n";
                }

                ofs << "    ]\n"; // values
                // ========= values ここまで =========

                ofs << "  }\n"; // volume
                ofs << "}\n";
                ofs.close();

                std::cout << "[Rank " << rank << "] wrote volume (with values) to "
                        << json_path << std::endl;
            }
            else
            {
                std::cerr << "Error: Cannot open " << json_path << " for writing.\n";
            }

            // Setup a transfer function.
            const auto min_value = o->minValue();
            const auto max_value = o->maxValue();

            auto omap = kvs::OpacityMap();
            omap.addPoint(   0.0, 0.0 );
            omap.addPoint(   20.0, 0.1 );
            omap.addPoint(   35.0, 0.7 );
            omap.addPoint(   50.0, 0.0 );
            omap.addPoint( 250.0, 0.6 );
            omap.addPoint( 253.0, 0.3 );
            omap.addPoint( 255.0, 0.2 );
            omap.create();

            auto cmap = adaptor->colorMap();
            auto tfunc = kvs::TransferFunction( cmap, omap );
            tfunc.setRange( min_value, max_value );

            // Register object and renderer to screen
            if ( screen.scene()->hasObject( "Volume" ) )
            {
                // Update the objects.
                screen.scene()->replaceObject( "Volume", o );
            }
            else
            {

                // Register the objects with renderer.
                auto* r = new kvs::glsl::RayCastingRenderer();
                //r->setShader( kvs::Shader::BlinnPhong() );
                r->setTransferFunction( tfunc );
                screen.registerObject( o, r );
            }


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
        };
    };
};

/*===========================================================================*/
/*
 * C functions for the Fortran module
 */
/*===========================================================================*/
extern "C"
{

Adaptor* InSituVis_new( const int method )
{
    const auto cmap = kvs::ColorMap::CoolWarm();
    const auto sub_image = Params::Output::SubImage;
    const auto sub_depth = Params::Output::SubImageDepth;
    const auto sub_alpha = Params::Output::SubImageAlpha;

    auto* vis = new Adaptor();

    // ▼▼ ここを追加：出力ベース/サブディレクトリを InSituVis.cpp から指定 ▼▼
    // 例）環境や用途に合わせてここだけ書き換えれば良い
    {
        std::string base_dir =  "/data2/tomoya/SmokeRing/GetVolume";
        std::string sub_dir  = "Process";

        vis->outputDirectory().setBaseDirectoryName( base_dir );
        vis->outputDirectory().setSubDirectoryName( sub_dir );
    }

    vis->setOutputImageEnabled( Params::Output::Image );
    vis->setOutputSubImageEnabled( sub_image, sub_depth, sub_alpha );
    vis->setImageSize( Params::ImageSize.x(), Params::ImageSize.y() );
    // vis->setViewpoint( Params::Viewpoint );
    vis->setAnalysisInterval( Params::AnalysisInterval );
    vis->setColorMap( cmap );

    // （重要）複数 Viewpoint + 保存名を登録
    vis->addViewpoint( Params::Viewpoint1, "181" );
    vis->addViewpoint( Params::Viewpoint2, "182" );
    vis->addViewpoint( Params::Viewpoint3, "142" );
    vis->addViewpoint( Params::Viewpoint4, "143" );
    vis->addViewpoint( Params::Viewpoint5, "1201" );

    switch ( method )
    {
    case 1: vis->setPipeline( Adaptor::OrthoSlice( vis ) ); break;
    case 2: vis->setPipeline( Adaptor::Isosurface( vis ) ); break;
    case 3:
    {
        // vis->screen().setBackgroundColor( kvs::RGBColor::Black() );
        vis->setAlphaBlendingEnabled( true );
        vis->setPipeline( Adaptor::VolumeRendering( vis ) );
        break;
    }
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

void InSituVis_setGlobalDims( Adaptor* self, int dimx, int dimy, int dimz )
{
    self->setGlobalDims( kvs::Vec3ui( dimx, dimy, dimz ) );
}

void InSituVis_setOffset( Adaptor* self, int offx, int offy, int offz )
{
    self->setOffset( kvs::Vec3ui( offx, offy, offz ) );
}

void InSituVis_put( Adaptor* self, double* values, int dimx, int dimy, int dimz )
{
    const auto dims = kvs::Vec3ui( dimx, dimy, dimz );
    const auto size = size_t( dimx * dimy * dimz );
    const auto offs = self->offset();
    const auto min_coord = kvs::Vec3{ offs };
    const auto max_coord = kvs::Vec3{ offs + dims } - kvs::Vec3{ 1, 1, 1 };

    Adaptor::Volume volume;
    volume.setVeclen( 1 );
    volume.setResolution( dims );
    volume.setValues( kvs::ValueArray<double>{ values, size } );
    volume.setGridTypeToUniform();
    volume.updateMinMaxValues();
    volume.setMinMaxObjectCoords( min_coord, max_coord );
    volume.setMinMaxExternalCoords( min_coord, max_coord );

    self->put( volume );
}

void InSituVis_exec( Adaptor* self, double time_value, int time_index )
{
    self->exec( { float( time_value ), size_t( time_index ) } );
}

} // end of extern "C"
