#pragma once
#if defined( KVS_SUPPORT_MPI )
#undef KVS_SUPPORT_MPI
#endif
#include <InSituVis/Lib/Adaptor.h>

extern "C" InSituVis::Adaptor* InSituVis_new();
extern "C" void InSituVis_delete( InSituVis::Adaptor* self );
extern "C" void InSituVis_initialize( InSituVis::Adaptor* self );
extern "C" void InSituVis_finalize( InSituVis::Adaptor* self );
extern "C" void InSituVis_put( InSituVis::Adaptor* self, double* values, int nvalues, int dimx, int dimy, int dimz );
extern "C" void InSituVis_exec( InSituVis::Adaptor* self, double time_value, long time_index );
