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
#include <kvs/StampTimer>
#include <kvs/StampTimerList>
#include <InSituVis/Lib/Adaptor.h>
#include <InSituVis/Lib/Viewpoint.h>
#include <InSituVis/Lib/CubicViewpoint.h>
#include <InSituVis/Lib/SphericalViewpoint.h>
#include <InSituVis/Lib/PolyhedralViewpoint.h>
#include <InSituVis/Lib/StochasticRenderingAdaptor.h>
#include <InSituVis/Lib/CameraPathControlledAdaptorMulti_mpi.h>

/*****************************************************************************/
// In-situ visualization settings
/*****************************************************************************/

const auto Pos = [] ( const float r )
{
    const auto tht = kvs::Math::pi / 4.0f;
    const auto phi = kvs::Math::pi / 4.0f;
    const auto x = r * std::sin( tht ) * std::sin( phi );
    const auto y = r * std::cos( tht );
    const auto z = r * std::sin( tht ) * std::cos( phi );
    return kvs::Vec3{ x, y, z };
};
using AdaptorBase = InSituVis::mpi::CameraPathControlledAdaptorMulti;

// Parameters
//----------------------------------------------------------------------------
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
    static const auto FrameEntropies = true;
    static const auto ZoomEntropies = true;
};
const auto VisibleBoundingBox = false;
const auto VisibleSubBoundingBox = false;
const auto AutoZoom = true;
const auto ColorImage = true;

const auto ImageSize = kvs::Vec2ui{ 512, 512 }; // width x height
const auto AnalysisInterval = 10; // l: analysis (visuaization) time interval
const auto entropyInterval = 10;
const auto FocusPointCandidates = 3;
const auto ViewPointCandidates = 3;

// For IN_SITU_VIS__VIEWPOINT__*
//const auto ViewPos = kvs::Vec3{-7.0f,0.0f,1.0f}; 
// const auto ViewPos = kvs::Vec3{-8.0f,-4.0f,-6.0f}; // viewpoint position
const auto ViewPos = kvs::Vec3{-5.0f,-2.0f,5.0f}; // viewpoint position
// const auto ViewPos = kvs::Vec3{0.0f,0.0f,12.0f}; // viewpoint position

const auto ViewDim = kvs::Vec3ui{ 1, 9, 20 }; // viewpoint dimension
//const auto ViewDim = kvs::Vec3ui{ 1, 35, 70 }; // viewpoint dimension
const auto ViewDir = InSituVis::Viewpoint::Direction::Uni; // Uni or Omni
//add


kvs::Vec3 m_base_position = {0.0f,12.0f,0.0f};
auto xyz_to_rtp = [&] ( const kvs::Vec3& xyz ) -> kvs::Vec3 {
    const float x = xyz[0];
    const float y = xyz[1];
    const float z = xyz[2];
    const float r = sqrt( x * x + y * y + z * z );
    const float t = std::acos( y / r );
    const float p = std::atan2( x, z );
    return kvs::Vec3( r, t, p );
};
auto calc_rotation = [&] ( const kvs::Vec3& xyz ) -> kvs::Quaternion {
    const auto rtp = xyz_to_rtp( xyz );
    const float phi = rtp[2];
    const auto axis = kvs::Vec3( { 0.0f, 1.0f, 0.0f } );
    auto q_phi = kvs::Quaternion( axis, phi );
    const auto q_theta = kvs::Quaternion::RotationQuaternion( m_base_position, xyz );
    return q_theta * q_phi;
};
auto rotation = calc_rotation(ViewPos);

const auto Viewpoint = InSituVis::Viewpoint{ { 000000, ViewDir, ViewPos , kvs::Vec3{ 0.0f,1.0f,0.0f }, rotation} };
//const auto Viewpoint = InSituVis::Viewpoint{ { ViewDir, ViewPos } };
const auto ViewpointSpherical = InSituVis::SphericalViewpoint{ ViewDim, ViewDir };
const auto ViewpointPolyhedral = InSituVis::PolyhedralViewpoint{ ViewDim, ViewDir };

// For IN_SITU_VIS__ADAPTOR__CAMERA_FOCUS_CONTROLL
const auto ZoomLevel = 5;
const auto FrameDivs = kvs::Vec2ui{ 20, 20 };
const auto MixedRatio = 0.5f; // mixed entropy ratio
auto LightEnt = ::AdaptorBase::LightnessEntropy();
auto DepthEnt = ::AdaptorBase::DepthEntropy();
auto MixedEnt = ::AdaptorBase::MixedEntropy( LightEnt, DepthEnt, MixedRatio );

// Entropy function
auto EntropyFunction = MixedEnt;
//auto EntropyFunction = LightEnt;
//auto EntropyFunction = DepthEnt;

// Path interpolator
const auto InterpolationMethod = ::AdaptorBase::InterpolationMethod::SLERP;

// How to evaluate ROI
// auto ROImethod = ::Adaptor::ROIMethod::max;
auto ROImethod = ::AdaptorBase::ROIMethod::maximum; 

// For IN_SITU_VIS__ADAPTOR__STOCHASTIC_RENDERING

}

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
    kvs::mpi::StampTimer m_sim_timer{ BaseClass::world() }; ///< timer for sim. process
    kvs::mpi::StampTimer m_vis_timer{ BaseClass::world() }; ///< timer for vis. process
    size_t m_final_time_step_index = 0;


public:
    Adaptor() = default;
    virtual ~Adaptor() = default;

    const kvs::Vec3ui globalDims() const { return m_global_dims; }
    const kvs::Vec3ui offset() const { return m_offset; }
    const kvs::ColorMap colorMap() const { return m_cmap; }

    void setGlobalDims( const kvs::Vec3ui& dims ) { m_global_dims = dims; }
    void setOffset( const kvs::Vec3ui& offs ) { m_offset = offs; }
    void setColorMap( const kvs::ColorMap& cmap ) { m_cmap = cmap; }

    kvs::mpi::StampTimer& simTimer() { return m_sim_timer; }
    kvs::mpi::StampTimer& visTimer() { return m_vis_timer; }

    void exec( const SimTime sim_time )
    {
        this->set_min_max_values();
        this->set_global_bounds();
        BaseClass::exec( sim_time );
    }
    void setFinalTimeStepIndex( size_t index )
    {
        m_final_time_step_index = index;
        this->setFinalTimeStep( m_final_time_step_index );
    }

    void execRendering()
    {

        if ( !Params::VisibleBoundingBox ){
            BaseClass::execRendering();
            return;

        }
        BaseClass::execRendering();
        auto min_coord = kvs::Vec3{ 0, 0, 0 };
        auto max_coord = kvs::Vec3{ m_global_dims } - kvs::Vec3{ 1, 1, 1 };

        Object dummy;
        dummy.setMinMaxObjectCoords( min_coord, max_coord );
        dummy.setMinMaxExternalCoords( min_coord, max_coord );

        kvs::Bounds bounds( kvs::RGBColor::Black(), 2.0f );
        auto* bbox = bounds.outputLineObject( &dummy );
        bbox->setName( "BoundingBox" );
        BaseClass::screen().registerObject( bbox );
        const bool visible = BaseClass::world().isRoot();
        if ( bbox ) { bbox->setVisible( visible && Params::VisibleBoundingBox ); }
        if ( BaseClass::isEntStep() && !BaseClass::isErpStep() )
        {
            const auto index = BaseClass::maxIndex();
            for( size_t i = 0; i < BaseClass::focusPointCandidateNum()*BaseClass::viewPointCandidateNum(); i++ ){
                const auto focus = BaseClass::candFocusPoints()[i];
                // const auto zoom_level = BaseClass::candZoomLevels()[i];
                auto location = BaseClass::focusedLocation( BaseClass::viewpoint().at( index ), focus );
                auto p = location.position;
                for ( size_t level = 0; level < BaseClass::zoomLevel(); level++ ){
                    // Update camera position.
                    auto t = static_cast<float>( level ) / static_cast<float>( BaseClass::zoomLevel() );
                    location.position = ( 1 - t ) * p + t * focus;
                    // Output the rendering images and the heatmap of entropies.
                    if ( Params::AutoZoom == false )
                    {                            
                        auto frame_buffer =  BaseClass::readback( location );
                        if ( BaseClass::world().isRoot() )
                        {
                            if ( BaseClass::isOutputImageEnabled() )
                            {                                                    
                                if ( Params::ColorImage ) BaseClass::outputColorImage( location, frame_buffer, i, level, 0 );
                                else {BaseClass::outputDepthImage( location, frame_buffer, i, level, 0 );}
                            }
                        }
                    }
            }
            if ( Params::AutoZoom )
            {   
                //auto location = BaseClass::focusedLocation( BaseClass::viewpoint().at( index ) , focus ); 
                location.position = BaseClass::candPositions()[i];
                auto level = BaseClass::candZoomLevels()[i];
                auto frame_buffer = BaseClass::readback( location );
                if ( BaseClass::world().isRoot() )
                {
                    if ( BaseClass::isOutputImageEnabled() )
                    {
                        if ( Params::ColorImage ) BaseClass::outputColorImage( location, frame_buffer, i, level, 0 );
                        else {BaseClass::outputDepthImage( location, frame_buffer, i, level, 0 );}
                    }
                }
            }
            }
        }
        else
        {
            const auto focus = BaseClass::erpFocus();
            auto location = BaseClass::erpLocation( focus );
            const auto zoom_level = BaseClass::zoomLevel();
            const auto p = location.position;
            for ( size_t level = 0; level < BaseClass::zoomLevel(); level++ )
            {
                auto t = static_cast<float>( level ) / static_cast<float>( BaseClass::zoomLevel() );
                location.position = ( 1 - t ) * p + t * focus;

                if ( Params::AutoZoom == false )
                {
                    auto frame_buffer = BaseClass::readback( location );
                    // Output the rendering images and the heatmap of entropies.
                    if ( BaseClass::world().isRoot() )
                    {
                        if ( BaseClass::isOutputImageEnabled() )
                        {   
                            if ( Params::ColorImage ) BaseClass::outputColorImage( location, frame_buffer, 999999, level, BaseClass::routeNum());
                            else {BaseClass::outputDepthImage( location, frame_buffer, 999999, level, BaseClass::routeNum()  );}
                        }
                    }
                }
            }
            if(Params::AutoZoom)
            {
                //const auto focus = BaseClass::erpFocus();
                auto locatio = BaseClass::erpLocation( focus );
                //location.position = BaseClass::bestLocationPosition();
                //auto bestlocation = BaseClass::erpLocation( focus );
                //auto location = BaseClass::bestLocation();
                // auto location = BaseClass::estimatedZoomLocation();
                auto frame_buffer = BaseClass::readback( locatio );

                // Output the rendering images and the heatmap of entropies.
                if ( BaseClass::world().isRoot() )
                {
                    if ( BaseClass::isOutputImageEnabled() )
                    {  
                        if ( Params::ColorImage ) BaseClass::outputColorImage( location, frame_buffer, 999999, 000000, BaseClass::routeNum() );
                        else {BaseClass::outputDepthImage(location, frame_buffer, 999999, 000000, BaseClass::routeNum() );}
                    }
                }
            }
        }
        if ( bbox ) { bbox->setVisible( false ); }
    }

    bool dump()
    {
        if ( !BaseClass::dump() ) return false;

        // For each node
        m_sim_timer.setTitle( "Sim time" );
        m_vis_timer.setTitle( "Vis time" );

        const std::string rank = kvs::String::From( this->world().rank(), 4, '0' );
        const std::string subdir = BaseClass::outputDirectory().name() + "/";
        kvs::StampTimerList timer_list;
        timer_list.push( m_sim_timer );
        timer_list.push( m_vis_timer );
        if ( !timer_list.write( subdir + "proc_time_" + rank + ".csv" ) ) return false;

        // For root node
        auto sim_time_min = m_sim_timer; sim_time_min.reduceMin();
        auto sim_time_max = m_sim_timer; sim_time_max.reduceMax();
        auto sim_time_ave = m_sim_timer; sim_time_ave.reduceAve();
        auto vis_time_min = m_vis_timer; vis_time_min.reduceMin();
        auto vis_time_max = m_vis_timer; vis_time_max.reduceMax();
        auto vis_time_ave = m_vis_timer; vis_time_ave.reduceAve();

        if ( !this->world().isRoot() ) return true;

        sim_time_min.setTitle( "Sim time (min)" );
        sim_time_max.setTitle( "Sim time (max)" );
        sim_time_ave.setTitle( "Sim time (ave)" );
        vis_time_min.setTitle( "Vis time (min)" );
        vis_time_max.setTitle( "Vis time (max)" );
        vis_time_ave.setTitle( "Vis time (ave)" );

        timer_list.clear();
        timer_list.push( sim_time_min );
        timer_list.push( sim_time_max );
        timer_list.push( sim_time_ave );
        timer_list.push( vis_time_min );
        timer_list.push( vis_time_max );
        timer_list.push( vis_time_ave );

        const auto basedir = BaseClass::outputDirectory().baseDirectoryName() + "/";
        return timer_list.write( basedir + "proc_time.csv" );
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
                // Bounding box.
                //screen.registerObject( o0, new kvs::Bounds() );

                // Register the objects with renderer.
                auto* r = new kvs::glsl::PolygonRenderer();
                r->setTwoSideLightingEnabled( true );
                screen.registerObject( o, r );
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
    vis->setOutputImageEnabled(
        Params::Output::Image );
    vis->setOutputSubImageEnabled(
        Params::Output::SubImage,
        Params::Output::SubImageDepth,
        Params::Output::SubImageAlpha );
    vis->setOutputEntropiesEnabled(
        Params::Output::Entropies );
    vis->setOutputFrameEntropiesEnabled(
        Params::Output::FrameEntropies );
    vis->setOutputEvaluationImageEnabled(
        Params::Output::EvalImage,
        Params::Output::EvalImageDepth );
    vis->setImageSize( Params::ImageSize.x(), Params::ImageSize.y() );
    vis->setViewpoint( Params::ViewpointSpherical );
    vis->setAnalysisInterval( Params::AnalysisInterval );
    vis->setColorMap( kvs::ColorMap::CoolWarm() );
    vis->setZoomLevel( Params::ZoomLevel );
    vis->setFrameDivisions( Params::FrameDivs );
    vis->setEntropyFunction( Params::EntropyFunction );
    vis->setOutputColorImage( Params::ColorImage );
    vis->setAutoZoomingEnabled( Params::AutoZoom );

    vis->setColorMap( kvs::ColorMap::CoolWarm() );
    vis->setEntropyInterval( Params::entropyInterval );
    vis->setInterpolationMethod( Params::InterpolationMethod );
    //add
    vis->setROIMethod( Params::ROImethod );
    vis->setFocusPointCandidateNum( Params::FocusPointCandidates );
    vis->setViewPointCandidateNum( Params::ViewPointCandidates );
    vis->setViewDim( Params::ViewDim );



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

void InSituVis_setFinalTimeStep( Adaptor* self, int index )
{
    self->setFinalTimeStep( index );
}

void InSituVis_simTimerStart( Adaptor* self )
{
    self->simTimer().start();
}

void InSituVis_simTimerStamp( Adaptor* self )
{
    self->simTimer().stamp();
}

void InSituVis_visTimerStart( Adaptor* self )
{
    self->visTimer().start();
}

void InSituVis_visTimerStamp( Adaptor* self )
{
    self->visTimer().stamp();
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
