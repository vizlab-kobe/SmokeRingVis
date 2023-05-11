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


// Parameters
namespace Params
{
const auto ImageSize = kvs::Vec2ui{ 512, 512 }; // width x height
const auto AnalysisInterval = 100; // analysis (visuaization) time interval
const auto ViewPos = kvs::Vec3{ 7, 5, 6 }; // viewpoint position
const auto ViewDir = InSituVis::Viewpoint::Direction::Uni; // Uni or Omni
const auto Viewpoint = InSituVis::Viewpoint{ { ViewDir, ViewPos } }; // viewpoint
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
    return [cmap] ( Screen& screen, const Object& object )
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
    return [cmap] ( Screen& screen, const Object& object )
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
    return [cmap] ( Screen& screen, const Object& object )
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
            screen.registerObject( o, new kvs::Bounds() );

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
        }
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

    const auto cmap = kvs::ColorMap::CoolWarm();
    switch ( method )
    {
    case 1: vis->setPipeline( OrthoSlice( cmap ) ); break;
    case 2: vis->setPipeline( Isosurface( cmap ) ); break;
    case 3: vis->setPipeline( VolumeRendering( cmap ) ); break;
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
