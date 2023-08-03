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
#include <InSituVis/Lib/CameraFocusControlledAdaptor_mpi.h>
#include <InSituVis/Lib/CFCA.h>

/*****************************************************************************/
// In-situ visualization settings
/*****************************************************************************/

// Adaptor setting
//----------------------------------------------------------------------------
#define IN_SITU_VIS__ADAPTOR__CAMERA_PATH_CONTROLL
//#define IN_SITU_VIS__ADAPTOR__STOCHASTIC_RENDERING

// Pipeline setting
//----------------------------------------------------------------------------
//#define IN_SITU_VIS__PIPELINE__ORTHO_SLICE
#define IN_SITU_VIS__PIPELINE__ISOSURFACE
//#define IN_SITU_VIS__PIPELINE__EXTERNAL_FACE
//#define IN_SITU_VIS__PIPELINE__WHOLE_MIN_MAX_VALUES

// Viewpoint setting
//----------------------------------------------------------------------------
#define IN_SITU_VIS__VIEWPOINT__SINGLE
//#define IN_SITU_VIS__VIEWPOINT__MULTIPLE_SPHERICAL
//#define IN_SITU_VIS__VIEWPOINT__MULTIPLE_POLYHEDRAL

// Base adaptor
//using AdaptorBase = InSituVis::mpi::CFCA;
using AdaptorBase = InSituVis::mpi::CameraFocusControlledAdaptor;

inline const kvs::Vec3 Pos( const float r )
{
    const auto tht = kvs::Math::pi / 4.0f;
    const auto phi = kvs::Math::pi / 4.0f;
    const auto x = static_cast<float>( r * std::sin( tht ) * std::sin( phi ) );
    const auto y = static_cast<float>( r * std::cos( tht ) );
    const auto z = static_cast<float>( r * std::sin( tht ) * std::cos( phi ) );
    return kvs::Vec3{ x, y, z };
};

inline kvs::Quaternion Rot( const kvs::Vec3& base, const kvs::Vec3& xyz )
{
    auto xyz_to_rtp = [] ( const kvs::Vec3& m )
    {
        const float x = m[0];
        const float y = m[1];
        const float z = m[2];
        const float r = sqrt( x * x + y * y + z * z );
        const float t = std::acos( y / r );
        const float p = std::atan2( x, z );
        return kvs::Vec3( r, t, p );
    };

    const auto rtp = xyz_to_rtp( xyz );
    const float phi = rtp[2];
    const auto axis = kvs::Vec3( { 0.0f, 1.0f, 0.0f } );
    const auto q_phi = kvs::Quaternion( axis, phi );
    const auto q_theta = kvs::Quaternion::RotationQuaternion( base, xyz );
    return q_theta * q_phi;
};

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
    static const auto FrameEntropies = true;
};

const auto VisibleBoundingBox = true;
const auto kotei = false;

const auto ImageSize = kvs::Vec2ui{ 512, 512 }; // width x height
const auto AnalysisInterval = 10; // analysis (visuaization) time interval
const auto BasePos = kvs::Vec3{ 0.0f, 12.0f, 0.0f };
//onst auto ViewPos = kvs::Vec3{ 7, 5, 6 }; // viewpoint position
//const auto ViewRad = 12.0f; // viewpoint radius
//const auto ViewPos = Pos( ViewRad ); // viewpoint position
const auto ViewPos = kvs::Vec3{ -3.0f, 0.6f, 1.8f }; // viewpoint position
const auto ViewDir = InSituVis::Viewpoint::Direction::Uni; // Uni or Omni
const auto ViewDim = kvs::Vec3ui{ 1, 5, 10 }; // viewpoint dimension
const auto Viewpoint = InSituVis::Viewpoint{ { 000000, ViewDir, ViewPos , kvs::Vec3{ 0, 1, 0 }, Rot( BasePos, ViewPos ) } };
const auto ViewpointSpherical = InSituVis::SphericalViewpoint{ ViewDim, ViewDir };
const auto ViewpointPolyhedral = InSituVis::PolyhedralViewpoint{ ViewDim, ViewDir };

// For IN_SITU_VIS__ADAPTOR__CAMERA_FOCUS_CONTROLL
const auto ZoomLevel = 5;
const auto FrameDivs = kvs::Vec2ui{ 10, 10 };
const auto EntropyInterval = 1; // L: entropy calculation time interval
//const auto EntropyInterval = 2; // L: entropy calculation time interval
const auto MixedRatio = 0.5f; // mixed entropy ratio
//const auto MixedRatio = 0.75f; // mixed entropy ratio
auto LightEnt = AdaptorBase::LightnessEntropy();
auto DepthEnt = AdaptorBase::DepthEntropy();
auto MixedEnt = AdaptorBase::MixedEntropy( LightEnt, DepthEnt, MixedRatio );

// Entropy function
auto EntropyFunction = MixedEnt;
//auto EntropyFunction = LightEnt;
//auto EntropyFunction = DepthEnt;

// Path interpolator
auto Interpolator = AdaptorBase::Squad();
//auto Interpolator = ::Adaptor::Slerp();

// For IN_SITU_VIS__ADAPTOR__STOCHASTIC_RENDERING
const auto Repeats = 50; // number of repetitions for stochastic rendering
const auto BoundaryMeshOpacity = 30; // opacity value [0-255] of boundary mesh
} // end of namespace Params


// Adaptor
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

public:
    Adaptor() = default;
    virtual ~Adaptor() = default;

    const kvs::Vec3ui globalDims() const { return m_global_dims; }
    const kvs::Vec3ui offset() const { return m_offset; }
    const kvs::ColorMap colorMap() const { return m_cmap; }

    void setGlobalDims( const kvs::Vec3ui& dims ) { m_global_dims = dims; }
    void setOffset( const kvs::Vec3ui& offs ) { m_offset = offs; }
    void setColorMap( const kvs::ColorMap& cmap ) { m_cmap = cmap; }
    void setFinalTimeStepIndex( const size_t index )
    {
#if defined( IN_SITU_VIS__ADAPTOR__CAMERA_PATH_CONTROLL )
        BaseClass::setFinalTimeStep( index );
#endif
    }

    void exec( const SimTime sim_time )
    {
        this->set_min_max_values();
        this->set_global_bounds();
        BaseClass::exec( sim_time );
    }

#if 0
    void execRendering()
    {
        if ( !Params::VisibleBoundingBox )
        {
            BaseClass::execRendering();
            return;
        }

        auto* bbox = kvs::LineObject::DownCast( BaseClass::screen().scene()->object( "Bounds" ) );
        if ( bbox && Params::VisibleBoundingBox ) { bbox->setVisible( false ); }

        BaseClass::execRendering();

        const bool visible = BaseClass::world().isRoot();
        if ( bbox ) { bbox->setVisible( visible && Params::VisibleBoundingBox ); }

        if ( BaseClass::isEntropyStep() )
        {
            const auto index = BaseClass::maxIndex();
            const auto focus = BaseClass::maxFocusPoint();
            auto location = BaseClass::viewpoint().at( index );
            //add
            auto bestlocation = BaseClass::focusedLocation( location , focus );
            bestlocation.position = BaseClass::bestLocationPosition();
            bestlocation.rotation = BaseClass::maxRotation();
            //auto location = BaseClass::bestLocation();

            const auto level = BaseClass::bestZoomLevel();
            auto frame_buffer = BaseClass::readback( bestlocation );
            // Output the rendering images and the heatmap of entropies.
            if ( BaseClass::world().isRoot() )
            {
                if ( BaseClass::isOutputImageEnabled() )
                {
                    BaseClass::outputColorImage( bestlocation, frame_buffer, level );
                }
            }
        }
        else
        {
            const auto focus = BaseClass::erpFocus();
            auto location = BaseClass::erpLocation( focus );
            //location.position = BaseClass::bestLocationPosition();
            //auto bestlocation = BaseClass::erpLocation( focus );
            //auto location = BaseClass::bestLocation();
            const auto level = BaseClass::bestZoomLevel();
            auto frame_buffer = BaseClass::readback( location );

            // Output the rendering images and the heatmap of entropies.
            if ( BaseClass::world().isRoot() )
            {
                if ( BaseClass::isOutputImageEnabled() )
                {
                    BaseClass::outputColorImage( location, frame_buffer, level );
                }
            }
        }

        /*if ( BaseClass::isEntropyStep() )
        {
            const auto index = BaseClass::maxIndex();
            const auto focus = BaseClass::maxFocusPoint();
            auto location = BaseClass::focusedLocation( BaseClass::viewpoint().at( index ), focus );

            const auto zoom_level = BaseClass::zoomLevel();
            const auto p = location.position;
            for ( size_t level = 0; level < zoom_level; level++ )
            {
                auto t = static_cast<float>( level ) / static_cast<float>( zoom_level );
                location.position = ( 1 - t ) * p + t * focus;
                //注視点を固定させたい時
                if(Params::kotei == true){
                location.look_at = {0,0,0};
                location.up_vector = {0,1,0};
                }
                auto frame_buffer = BaseClass::readback( location );

                // Output the rendering images and the heatmap of entropies.
                if ( BaseClass::world().isRoot() )
                {
                    if ( BaseClass::isOutputImageEnabled() )
                    {
                        BaseClass::outputColorImage( location, frame_buffer, level );
                        //BaseClass::outputDepthImage( location, frame_buffer, level );
                    }
                }
            }
        }
        else
        {
            const auto focus = BaseClass::maxFocusPoint();
            auto location = BaseClass::erpLocation( focus );

            const auto zoom_level = BaseClass::zoomLevel();
            const auto p = location.position;
            for ( size_t level = 0; level < zoom_level; level++ )
            {
                auto t = static_cast<float>( level ) / static_cast<float>( zoom_level );
                location.position = ( 1 - t ) * p + t * focus;
                //注視点を固定させたい時
                if(Params::kotei == true){
                location.look_at = {0,0,0};
                location.up_vector = {0,1,0};
                }
                auto frame_buffer = BaseClass::readback( location );

                // Output the rendering images and the heatmap of entropies.
                if ( BaseClass::world().isRoot() )
                {
                    if ( BaseClass::isOutputImageEnabled() )
                    {
                        BaseClass::outputColorImage( location, frame_buffer, level );
                        //BaseClass::outputDepthImage( location, frame_buffer, level );
                    }
                }
            }
        }*/
    }
#endif

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

//            const bool visible = BaseClass::isAlphaBlendingEnabled() ? false : BaseClass::world().isRoot();
            const bool visible = false;
            kvs::Bounds bounds( kvs::RGBColor::Black(), 2.0f );
            auto* object = bounds.outputLineObject( &dummy );
            object->setName( "Bounds" );
            object->setVisible( visible );
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
            //auto i1 = kvs::Math::Mix( min_value, max_value, 0.2 );
            //auto i2 = kvs::Math::Mix( min_value, max_value, 0.5 );
            //auto i3 = kvs::Math::Mix( min_value, max_value, 0.9 );
            auto* o0 = new kvs::Isosurface( &volume, i0, n, d, t );
            //auto* o1 = new kvs::Isosurface( &volume, i1, n, d, t );
            //auto* o2 = new kvs::Isosurface( &volume, i2, n, d, t );
            //auto* o3 = new kvs::Isosurface( &volume, i3, n, d, t );
            o0->setName( "Isosurface0" );
            //o1->setName( "Isosurface1" );
            //o2->setName( "Isosurface2" );
            //o3->setName( "Isosurface3" );

            // Register object and renderer to screen
            kvs::Light::SetModelTwoSide( true );
            if ( screen.scene()->hasObject( "Isosurface0" ) )
            {
                // Update the objects.
                screen.scene()->replaceObject( "Isosurface0", o0 );
                //screen.scene()->replaceObject( "Isosurface1", o1 );
                //screen.scene()->replaceObject( "Isosurface2", o2 );
                //screen.scene()->replaceObject( "Isosurface3", o3 );
            }
            else
            {
                // Bounding box.
                //screen.registerObject( o0, new kvs::Bounds() );

                // Register the objects with renderer.
                auto* r0 = new kvs::glsl::PolygonRenderer();
                //auto* r1 = new kvs::glsl::PolygonRenderer();
                //auto* r2 = new kvs::glsl::PolygonRenderer();
                //auto* r3 = new kvs::glsl::PolygonRenderer();
                r0->setTwoSideLightingEnabled( true );
                //r1->setTwoSideLightingEnabled( true );
                //r2->setTwoSideLightingEnabled( true );
                //r3->setTwoSideLightingEnabled( true );
                screen.registerObject( o0, r0 );
                //screen.registerObject( o1, r1 );
                //screen.registerObject( o2, r2 );
                //screen.registerObject( o3, r3 );
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
    auto* vis = new Adaptor();
    vis->setImageSize( Params::ImageSize.x(), Params::ImageSize.y() );
    vis->setViewpoint( Params::Viewpoint );
    vis->setAnalysisInterval( Params::AnalysisInterval );
    vis->setOutputSubImageEnabled(
        Params::Output::SubImage,
        Params::Output::SubImageDepth,
        Params::Output::SubImageAlpha );
    vis->setColorMap( kvs::ColorMap::CoolWarm() );
    vis->setOutputEntropiesEnabled( Params::Output::Entropies );
    vis->setOutputFrameEntropiesEnabled( Params::Output::FrameEntropies );
    /*vis->setOutputEvaluationImageEnabled(
        Params::Output::EvalImage,
        Params::Output::EvalImageDepth );*/
    vis->setAnalysisInterval( Params::AnalysisInterval );
    vis->setZoomLevel( Params::ZoomLevel );
    vis->setFrameDivisions( Params::FrameDivs );
    vis->setEntropyInterval( Params::EntropyInterval );
    vis->setEntropyFunction( Params::EntropyFunction );
    vis->setInterpolator( Params::Interpolator );

#if defined( IN_SITU_VIS__VIEWPOINT__SINGLE )
        vis->setViewpoint( Params::Viewpoint );
#elif defined( IN_SITU_VIS__VIEWPOINT__MULTIPLE_SPHERICAL )
        vis->setViewpoint( Params::ViewpointSpherical );
#elif defined( IN_SITU_VIS__VIEWPOINT__MULTIPLE_POLYHEDRAL )
        vis->setViewpoint( Params::ViewpointPolyhedral );
#endif
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

void InSituVis_setFinalTimeStepIndex( Adaptor* self, int index )
{
    self->setFinalTimeStepIndex( index );
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
