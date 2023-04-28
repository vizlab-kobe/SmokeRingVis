!-------------------------------------------------------------------
! class-hpc-smoke-ring: A simple sample field solver.
!
!    by Akira Kageyama, Kobe University, Japan.
!       email: sgks@mac.com
!
!    This software is released under the MIT License.
!
!-------------------------------------------------------------------
!    src/main.f90
!-------------------------------------------------------------------

program main_m
!  use omp_lib
!  use constants_m   ! numerical constants
!  use rank_m        ! information for rank
!  use ut_m          ! utility functions
!  use params_m      ! parameters
!  use debug_m       ! for debugging
!  use grid_m        ! grid mesh
!  use field_m       ! field operators and operations
!  use slicedata_m   ! generate 2-d sliced data
!  use solver_m      ! 4th order runge-kutta integration method
!  use job_m         ! job monitor
  use smoke_ring_m
  use InSituVis_m

  implicit none

  integer(SI) :: myrank, nprocs, ierror
  integer(SI) :: myrank_x, myrank_y, myrank_z
  integer(SI) :: status(MPI_STATUS_SIZE)
  integer(SI) :: lower, upper
  real(DR) :: t0, t1

  integer(DI) :: nloop
  real(DR) :: dt, time

  type(field__fluid_t) :: fluid
  type(field__fluid_t) :: fluid_test

  type(rank__div_xyz_t) :: rank_div
  type(rank__peripheral_xyz_t) :: peripheral
  type(grid__div_range_xyz_t) :: grid_div_range

  ! IN_SITU_VIS: Adaptor setup
  ! {
  type( InSituVis ) :: insitu_vis
  integer :: dimx, dimy, dimz
  integer :: nvalues
  dimx = NX
  dimy = NY
  dimz = NZ
  nvalues = dimx * dimy * dimz
  ! }

  call mpi_init(ierror)
  call mpi_comm_rank(MPI_COMM_WORLD, myrank, ierror)
  call mpi_comm_size(MPI_COMM_WORLD, nprocs, ierror)
  call cpu_time(t0)

  call rank__initialize(myrank, rank_div)
  call rank__peripheral(rank_div, peripheral)
  call grid__determine_range(grid_div_range, rank_div)

  call params__read
  call grid%initialize(grid_div_range)
  call solver__initialize(fluid,grid_div_range,peripheral,status)
  call slicedata__initialize(myrank)

  time = 0.0_DR
  nloop = 0

  call solver__diagnosis(myrank,nloop,time,fluid,grid_div_range)
  dt = solver__set_time_step(nloop,fluid)

  ! IN_SITU_VIS: instance & initialize
  ! {
  insitu_vis = InSituVis()
  call insitu_vis%initialize()
  ! }

  do while(job__karte%state=="fine")
    call debug__print("running. nloop=",nloop)
    call solver__advance(time,dt,fluid,grid_div_range,peripheral,status)
    dt = solver__set_time_step(nloop,fluid)
    nloop = nloop + 1
    call solver__diagnosis(myrank,nloop,time,fluid,grid_div_range)
    call slicedata__write(nloop,time,fluid,myrank,grid_div_range,peripheral,status)

    ! IN_SITU_VIS: put & exec
    ! {
    call insitu_vis%put( fluid%pressure, nvalues, dimx, dimy, dimz )
    call insitu_vis%exec( time, nloop )
    ! }

    if (nloop>=params__get_integer('Total_nloop'))  &
      call job__karte%set("loop_max")
  end do
  call cpu_time(t1) 
  print *, "elappsed time", t1 - t0

  ! IN_SITU_VIS: finalize
  ! {
  call insitu_vis%finalize()
  ! }

  call mpi_finalize(ierror)
  call job__finalize(nloop)
end program main_m
