!-------------------------------------------------------------------
! class-hpc-smoke-ring: A simple sample field solver.
!
!    by Akira Kageyama, Kobe University, Japan.
!       email: sgks@mac.com
!
!    This software is released under the MIT License.
!
!-------------------------------------------------------------------
!    src/slicedata.f90
!-------------------------------------------------------------------

module slicedata_m
  use ut_m
  use field_m
  use params_m
  use debug_m
  use solver_m
  use grid_m
  use rank_m
  implicit none
  private
  public :: slicedata__initialize,  &
            slicedata__write

  ! - 2-D single precision real arrays.
  real(SR), dimension(:,:), allocatable :: Slice_vx  ! x-comp. of velocity
  real(SR), dimension(:,:), allocatable :: Slice_vy  ! y-comp.
  real(SR), dimension(:,:), allocatable :: Slice_vz  ! z-comp.
  real(SR), dimension(:,:), allocatable :: Slice_ps  ! Pressure
  real(SR), dimension(:,:), allocatable :: Slice_en  ! Enstrophy

  logical, save :: Initialize_done = .false.
  integer(SI), parameter :: FILE_SLICEDATA = 20

contains


  subroutine make_single_precision_field(vel,ps,grid_div_range,peripheral,status)
    
    type(field__vector3d_t),       intent(in) :: vel
    real(DR), dimension(NX,NY,NZ), intent(in) :: ps

    type(grid__div_range_xyz_t), intent(in) :: grid_div_range
    type(rank__peripheral_xyz_t), intent(in) :: peripheral
    integer(SI) :: status(MPI_STATUS_SIZE)
    integer(SI) :: ierror

    integer(SI) :: slice_j = NY / 2

    type(field__vector3d_t)       :: vor   ! vorticity
    real(DR), dimension(NX,NY,NZ) :: enstrophy

    
    real(SR), dimension(NX,NZ) :: Slice_vx_div  ! x-comp. of velocity
    real(SR), dimension(NX,NZ) :: Slice_vy_div  ! y-comp.
    real(SR), dimension(NX,NZ) :: Slice_vz_div  ! z-comp.
    real(SR), dimension(NX,NZ) :: Slice_ps_div  ! Pressure
    real(SR), dimension(NX,NZ) :: Slice_en_div  ! Enstrophy

!>        vor = .curl.vel
!>  enstrophy = vor.dot.vor
    call operator_curl(vor, vel, grid_div_range, peripheral, status)
    enstrophy = vor%x*vor%x +vor%y*vor%y + vor%z*vor%z
    !enstrophy = operator_dot_product(vor,vor)

    Slice_vx_div = real(    vel%x(:,slice_j,:),SR)
    Slice_vy_div = real(    vel%y(:,slice_j,:),SR)
    Slice_vz_div = real(    vel%z(:,slice_j,:),SR)
    Slice_ps_div = real(       ps(:,slice_j,:),SR)
    Slice_en_div = real(enstrophy(:,slice_j,:),SR)
     
    call mpi_reduce(Slice_vx_div(:,:), Slice_vx(:,:), NX*NZ, MPI_REAL4, MPI_SUM, &
      0,MPI_COMM_WORLD, ierror) 
    
    call mpi_reduce(Slice_vy_div(:,:), Slice_vy(:,:), NX*NZ, MPI_REAL4, MPI_SUM, &
      0,MPI_COMM_WORLD, ierror) 
    
    call mpi_reduce(Slice_vz_div(:,:), Slice_vz(:,:), NX*NZ, MPI_REAL4, MPI_SUM, &
      0,MPI_COMM_WORLD, ierror) 
    
    call mpi_reduce(Slice_ps_div(:,:), Slice_ps(:,:), NX*NZ, MPI_REAL4, MPI_SUM, &
      0,MPI_COMM_WORLD, ierror) 
    
    call mpi_reduce(Slice_en_div(:,:), Slice_en(:,:), NX*NZ, MPI_REAL4, MPI_SUM, &
      0, MPI_COMM_WORLD, ierror) 
    
    call debug__print('called slicedata/make_single_precision_field.')
  end subroutine make_single_precision_field


!
! Private
!===============
! Public
!


  subroutine slicedata__initialize(myrank)
    implicit none 
    integer(SI), intent(in) :: myrank
    allocate(Slice_vx(NX,NZ),   &
             Slice_vy(NX,NZ),   &
             Slice_vz(NX,NZ),   &
             Slice_ps(NX,NZ),   &
             Slice_en(NX,NZ))

    call debug__print('Slice data allocated.')
    
    if(myrank == 0) then
      open(FILE_SLICEDATA,  &
           file=trim(params__get_string('Slicedata_file')),  &
           form='unformatted')
    end if

    Initialize_done = .true.

    call debug__print('called slicedata__initlilize')
  end subroutine slicedata__initialize


  subroutine slicedata__write(nloop,time,fluid,myrank,grid_div_range,peripheral,status)
    integer(DI),          intent(in) :: nloop
    real(DR),             intent(in) :: time
    type(field__fluid_t), intent(in) :: fluid
    
    integer(SI), intent(in) :: myrank 
    type(grid__div_range_xyz_t), intent(in) :: grid_div_range
    type(rank__peripheral_xyz_t), intent(in) :: peripheral
    integer(SI) :: status(MPI_STATUS_SIZE)

    type(field__vector3d_t) :: vel

    if ( params__get_integer('Slicedata_nskip') <= 0 ) return
                                      ! Set zero or negative integer
                                      ! when you don't want to
                                      ! save any slice data.

    if ( mod(nloop,params__get_integer('Slicedata_nskip')) /= 0 ) return


    call ut__assert(Initialize_done,"<slicedata__write> Forgot init?")

    call solver__get_subfield(fluid,vel)
    
    call make_single_precision_field(vel,fluid%pressure, grid_div_range, & 
       peripheral, status)
    if(myrank == 0) then
      write(FILE_SLICEDATA) nloop, real(time,SR),  &
                            Slice_vx, Slice_vy, Slice_vz,  &
                            Slice_ps, Slice_en
    
    call ut__message('#slice data saved at ', nloop, time)
    end if

    call debug__print('called slicedata__write.')
  end subroutine slicedata__write

end module slicedata_m
