#include <InSituVis/Lib/Adaptor_mpi.h>
#include <InSituVis/Lib/Viewpoint.h>
#include <kvs/StructuredVolumeObject>
#include <kvs/LineObject>
#include <kvs/PolygonRenderer>
#include <kvs/RayCastingRenderer>
#include <kvs/Isosurface>
#include <kvs/OrthoSlice>
#include <kvs/Bounds>
#include <kvs/Coordinate>
#include <InSituVis/Lib/Adaptor.h>
#include <InSituVis/Lib/Viewpoint.h>
#include <InSituVis/Lib/CubicViewpoint.h>
#include <InSituVis/Lib/SphericalViewpoint.h>
#include <InSituVis/Lib/PolyhedralViewpoint.h>
#include <InSituVis/Lib/StochasticRenderingAdaptor.h>
#include <InSituVis/Lib/CameraPathControlledAdaptor_mpi.h>

// Parameters
namespace Params
{

struct Output
{
    static const auto Image = true;
    static const auto SubImage = false;
    static const auto SubImageDepth = false;
    static const auto SubImageAlpha = false;
    static const auto Entropies = true;
    static const auto EvalImage = false;
    static const auto EvalImageDepth = false;
};

const auto VisibleBoundingBox = true;
const auto VisibleSubBoundingBox = false;

const auto ImageSize = kvs::Vec2ui{ 512, 512 }; // width x height
const auto AnalysisInterval = 10; // analysis (visuaization) time interval

const auto ViewDim = kvs::Vec3ui{ 1, 9, 16 };
const auto ViewDir = InSituVis::Viewpoint::Direction::Uni; // Uni or Omni
const auto Viewpoint = InSituVis::SphericalViewpoint{ ViewDim, ViewDir };
// const auto Viewpoint = InSituVis::PolyhedralViewpoint{ ViewDim, ViewDir };

const auto Delta = 0.75f;
const auto entropyInterval = 30;
const auto MixedRatio = 0.5f; // mixed entropy ratio
auto LightEnt = InSituVis::mpi::CameraPathControlledAdaptor::LightnessEntropy();
auto DepthEnt = InSituVis::mpi::CameraPathControlledAdaptor::DepthEntropy();
auto MixedEnt = InSituVis::mpi::CameraPathControlledAdaptor::MixedEntropy( LightEnt, DepthEnt, MixedRatio );

// Entropy function
auto EntropyFunction = MixedEnt;
// auto EntropyFunction = LightEnt;
// auto EntropyFunction = DepthEnt;

// Path interpolator
// const auto InterpolationMethod = InSituVis::mpi::CameraPathControlledAdaptor::InterpolationMethod::SLERP;
const auto InterpolationMethod = InSituVis::mpi::CameraPathControlledAdaptor::InterpolationMethod::SQUAD;

} // end of namespace Params

// Adaptor
using AdaptorBase = InSituVis::mpi::CameraPathControlledAdaptor;
class Adaptor : public AdaptorBase
{
public:
    using BaseClass = AdaptorBase;
    using Screen = AdaptorBase::Screen;
    using Volume = kvs::StructuredVolumeObject;
    using Object = kvs::ObjectBase;

private:
    kvs::Vec3ui m_global_dims{ 0, 0, 0 };
    kvs::Vec3ui m_offset{ 0, 0, 0 };
    kvs::ColorMap m_cmap{ 256 };
    size_t m_final_time_step_index = 0;

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
    void setFinalTimeStepIndex( size_t index )
    {
        m_final_time_step_index = index;
    #if defined( IN_SITU_VIS__ADAPTOR__CAMERA_PATH_CONTROLL )
        this->setFinalTimeStep( m_final_time_step_index );
    #endif
    }

    void execRendering()
    {
        if ( !Params::VisibleBoundingBox )
        {
            BaseClass::execRendering();
            return;
        }

        auto* bbox = kvs::LineObject::DownCast( BaseClass::screen().scene()->object( "BoundingBox" ) );
        if ( bbox && Params::VisibleBoundingBox ) { bbox->setVisible( false ); }

        BaseClass::execRendering();

        const bool visible = BaseClass::world().isRoot();
        if ( bbox ) { bbox->setVisible( visible && Params::VisibleBoundingBox ); }

        if ( BaseClass::isEntStep() && !BaseClass::isErpStep() )
        {
            const auto index = BaseClass::maxIndex();
            const auto location = BaseClass::viewpoint().at( index );
            const auto frame_buffer = BaseClass::readback( location );
            if ( BaseClass::world().isRoot() )
            {
                if ( BaseClass::isOutputImageEnabled() )
                {
                    BaseClass::outputColorImage( location, frame_buffer );
                }
            }
        }
        else
        {
            const auto location = BaseClass::erpLocation();
            const auto frame_buffer = BaseClass::readback( location );
            if ( BaseClass::world().isRoot() )
            {
                if ( BaseClass::isOutputImageEnabled() )
                {
                    BaseClass::outputColorImage( location, frame_buffer );
                }
            }
        }
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
            object->setVisible( Params::VisibleBoundingBox );
            BaseClass::screen().registerObject( object );
        }
    }

public:
    static Pipeline OrthoSlice( const Adaptor* adaptor )
    {
        return [adaptor] ( Screen& screen, const Object& object )
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

    static Pipeline Isosurface( const Adaptor* adaptor )
    {
        return [adaptor] ( Screen& screen, const Object& object )
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

            auto i0 = kvs::Math::Mix( min_value, max_value, 0.1 );
            auto i1 = kvs::Math::Mix( min_value, max_value, 0.2 );
            auto i2 = kvs::Math::Mix( min_value, max_value, 0.5 );
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
                //screen.registerObject( o0, new kvs::Bounds() );

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

    static Pipeline VolumeRendering( const Adaptor* adaptor )
    {
        return [adaptor] ( Screen& screen, const Object& object )
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
                const auto min_value = o->minValue();
                const auto max_value = o->maxValue();

                auto omap = kvs::OpacityMap();
                omap.addPoint(   0.0, 0.0 );
                omap.addPoint(   1.0, 0.2 );
                omap.addPoint( 250.0, 0.5 );
                omap.addPoint( 253.0, 0.1 );
                omap.addPoint( 255.0, 0.2 );
                omap.create();

                auto cmap = adaptor->colorMap();
                auto tfunc = kvs::TransferFunction( cmap, omap );
                tfunc.setRange( min_value, max_value );

                // Register the objects with renderer.
                auto* r = new kvs::glsl::RayCastingRenderer();
                //r->setShader( kvs::Shader::BlinnPhong() );
                r->setTransferFunction( tfunc );
                screen.registerObject( o, r );
            }
        };
    };
};


extern "C"
{

Adaptor* InSituVis_new( const int method )
{
    auto vis = new Adaptor();
    vis->setImageSize( Params::ImageSize.x(), Params::ImageSize.y() );
    vis->setViewpoint( Params::Viewpoint );
    vis->setAnalysisInterval( Params::AnalysisInterval );
    vis->setOutputSubImageEnabled( Params::Output::SubImage, Params::Output::SubImageDepth, Params::Output::SubImageAlpha );
    vis->setColorMap( kvs::ColorMap::CoolWarm() );
    vis->setOutputEntropiesEnabled( Params::Output::Entropies );
    vis->setOutputEvaluationImageEnabled( Params::Output::EvalImage, Params::Output::EvalImageDepth );
    vis->setEntropyInterval( Params::entropyInterval );
    vis->setDelta( Params::Delta );
    vis->setEntropyFunction( Params::EntropyFunction );
    vis->setInterpolationMethod( Params::InterpolationMethod );
    switch ( method )
    {
    case 1: vis->setPipeline( Adaptor::OrthoSlice( vis ) ); break;
    case 2: vis->setPipeline( Adaptor::Isosurface( vis ) ); break;
    case 3:
    {
        vis->screen().setBackgroundColor( kvs::RGBColor::Black() );
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
