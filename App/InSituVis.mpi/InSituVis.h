#pragma once
#include <InSituVis/Lib/Adaptor_mpi.h>

extern "C" InSituVis::mpi::Adaptor* InSituVis_new();
extern "C" void InSituVis_delete( InSituVis::mpi::Adaptor* self );
extern "C" void InSituVis_initialize( InSituVis::mpi::Adaptor* self );
extern "C" void InSituVis_finalize( InSituVis::mpi::Adaptor* self );
extern "C" void InSituVis_put( InSituVis::mpi::Adaptor* self, double* values, int nvalues, int dimx, int dimy, int dimz );
extern "C" void InSituVis_exec( InSituVis::mpi::Adaptor* self, double time_value, long time_index );
