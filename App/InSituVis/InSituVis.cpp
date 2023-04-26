#include "InSituVis.h"
#include <InSituVis/Lib/Viewpoint.h>
#include <kvs/StructuredVolumeObject>
#include <kvs/PolygonRenderer>
#include <kvs/Isosurface>


namespace Params
{

// Type definition
using Volume = kvs::StructuredVolumeObject;
using Object = kvs::ObjectBase;
using Screen = InSituVis::Adaptor::Screen;

// Setting parameters
const auto ImageSize = kvs::Vec2ui{ 512, 512 }; // width x height
const auto AnalysisInterval = 5; // l: analysis (visuaization) time interval
const auto ViewPos = kvs::Vec3{ 7, 5, 6 }; // viewpoint position
const auto ViewDir = InSituVis::Viewpoint::Direction::Uni; // Uni or Omni
const auto Viewpoint = InSituVis::Viewpoint{ { ViewDir, ViewPos } }; // viewpoint

// Visualization pipeline
auto Isosurface = [&] ( Screen& screen, const Object& object )
{
    Volume volume; volume.shallowCopy( Volume::DownCast( object ) );

    // Setup a transfer function.
    const auto min_value = volume.minValue();
    const auto max_value = volume.maxValue();
    auto t = kvs::TransferFunction( kvs::ColorMap::BrewerSpectral() );
    t.setRange( min_value, max_value );

    // Create new object
    auto n = kvs::Isosurface::PolygonNormal;
    auto d = true;
    auto i = kvs::Math::Mix( min_value, max_value, 0.5 );
    auto* o = new kvs::Isosurface( &volume, i, n, d, t );
    o->setName( "Object" );

    // Register object and renderer to screen
    kvs::Light::SetModelTwoSide( true );
    if ( screen.scene()->hasObject( "Object" ) )
    {
        // Update the objects.
        screen.scene()->replaceObject( "Object", o );
    }
    else
    {
        // Register the objects with renderer.
        auto* r = new kvs::glsl::PolygonRenderer();
        r->setTwoSideLightingEnabled( true );
        screen.registerObject( o, r );
    }
};

} // end of namespace Params


extern "C" InSituVis::Adaptor* InSituVis_new()
{
    auto vis = new InSituVis::Adaptor();
    vis->setImageSize( Params::ImageSize.x(), Params::ImageSize.y() );
    vis->setViewpoint( Params::Viewpoint );
    vis->setPipeline( Params::Isosurface );
    return vis;
}

extern "C" void InSituVis_delete( InSituVis::Adaptor* self )
{
    if ( self ) delete self;
}

extern "C" void InSituVis_initialize( InSituVis::Adaptor* self )
{
    self->initialize();
}

extern "C" void InSituVis_finalize( InSituVis::Adaptor* self )
{
    self->finalize();
}

extern "C" void InSituVis_put( InSituVis::Adaptor* self, float* values, int nvalues, int dimx, int dimy, int dimz )
{
    Params::Volume volume;
    volume.setVeclen( 1 );
    volume.setResolution( { dimx, dimy, dimz } );
    volume.setValues( kvs::ValueArray<float>{ values, size_t( nvalues ) } );
    volume.setGridTypeToUniform();
    volume.updateMinMaxValues();

    self->put( volume );
}

extern "C" void InSituVis_exec( InSituVis::Adaptor* self, float time_value, int time_index )
{
    self->exec( { time_value, size_t( time_index ) } );
}

