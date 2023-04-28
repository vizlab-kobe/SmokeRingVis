#pragma once
#include <InSituVis/Lib/Adaptor_mpi.h>


extern "C"
{

InSituVis::mpi::Adaptor* InSituVis_new();
void InSituVis_delete( InSituVis::mpi::Adaptor* self );
void InSituVis_initialize( InSituVis::mpi::Adaptor* self );
void InSituVis_finalize( InSituVis::mpi::Adaptor* self );
void InSituVis_put( InSituVis::mpi::Adaptor* self, double* values, int nvalues, int dimx, int dimy, int dimz );
void InSituVis_exec( InSituVis::mpi::Adaptor* self, double time_value, long time_index );

} // end of extern "C"
