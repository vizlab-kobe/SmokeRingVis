module smoke_ring_m
  use constants_m   ! numerical constants
  use rank_m        ! information for rank
  use ut_m          ! utility functions
  use params_m      ! parameters
  use debug_m       ! for debugging
  use grid_m        ! grid mesh
  use field_m       ! field operators and operations
  use slicedata_m   ! generate 2-d sliced data
  use solver_m      ! 4th order runge-kutta integration method
  use job_m         ! job monitor
end module smoke_ring_m
