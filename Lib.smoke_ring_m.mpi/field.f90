!-------------------------------------------------------------------
! class-hpc-smoke-ring: A simple sample field solver.
!
!    by Akira Kageyama, Kobe University, Japan.
!       email: sgks@mac.com
!
!    This software is released under the MIT License.
!
!-------------------------------------------------------------------
!    src/field.f90
!-------------------------------------------------------------------

module field_m
  use constants_m
  use grid_m
  use mpi
  use rank_m

  implicit none

  public
  private :: &!<< assignments >>!&
             assignment_real_to_fluid,           &
             assignment_real_to_vector
  public  :: &!<< operators >>!&
             operator_cross_product,             &
             operator_curl,                      &
             operator_div,                       &
             operator_dot_product,               &
             operator_energyintegral,            &
             operator_fluid_add,                 &
             operator_fluid_times_real,          &
             operator_laplacian_scalar,          &
             operator_laplacian_vector,          &
             operator_real_times_fluid,          &
             operator_real_times_vector,         &
             operator_scalar_times_vector,       &
             operator_scalarintegral,            &
             operator_vector_add,                &
             operator_vector_divby_scalar,       &
             operator_vector_times_real,         &
             operator_vector_times_scalar
  private :: &!<< routines >>!&
             boundary_condition_fluid,           &
             boundary_condition_scalar,          &
             boundary_condition_vector

  interface field__boundary_condition
    module procedure boundary_condition_fluid,  &
                     boundary_condition_scalar, &
                     boundary_condition_vector
  end interface

  !--- << Types >> ---!

  type field__vector3d_t
    real(DR), dimension(NX,NY,NZ) :: x
    real(DR), dimension(NX,NY,NZ) :: y
    real(DR), dimension(NX,NY,NZ) :: z
  end type field__vector3d_t

  type field__fluid_t
    real(DR), dimension(NX,NY,NZ) :: pressure  ! fluid pressure(scaler)
    real(DR), dimension(NX,NY,NZ) :: density   ! mass density(scaler)
    type(field__vector3d_t)       :: flux      ! mass flux(vector)
  end type field__fluid_t

  !--- << Operators >> ---!

! interface operator(.curl.)
!    module procedure operator_curl
! end interface
!
! interface operator(.div.)
!    module procedure operator_div
! end interface
!
! interface operator(.energyintegral.)
!    module procedure operator_energyintegral
! end interface
!
! interface operator(.scalarintegral.)
!    module procedure operator_scalarintegral
! end interface
!
! interface operator(.laplacian.)
!    module procedure operator_laplacian_scalar
!    module procedure operator_laplacian_vector
! end interface
!
! interface operator(.x.)
!    module procedure operator_cross_product
! end interface
!
! interface operator(.dot.)
!    module procedure operator_dot_product
! end interface
!
! interface operator(+)
!    module procedure operator_fluid_add
!    module procedure operator_vector_add
! end interface
!
! interface operator(/)
!    module procedure operator_vector_divby_scalar
! end interface
!
! interface operator(*)
!    module procedure operator_integer_times_fluid
!    module procedure operator_fluid_times_integer
!    module procedure operator_fluid_times_real
!    module procedure operator_real_times_fluid
!    module procedure operator_real_times_vector
!    module procedure operator_scalar_times_vector
!    module procedure operator_vector_times_real
!    module procedure operator_vector_times_scalar
! end interface
!
! interface assignment(=)
!    module procedure assignment_real_to_fluid
!    module procedure assignment_real_to_vector
! end interface


contains


  subroutine assignment_real_to_fluid(fluid,real)
    type(field__fluid_t), intent(out) :: fluid
    real(DR),             intent(in)  :: real

    fluid%pressure(:,:,:) = real
    fluid%density(:,:,:)  = real
    fluid%flux%x(:,:,:)   = real
    fluid%flux%y(:,:,:)   = real
    fluid%flux%z(:,:,:)   = real
  end subroutine assignment_real_to_fluid


  subroutine assignment_real_to_vector(vector,real)
    type(field__vector3d_t), intent(out) :: vector
    real(DR),                intent(in)  :: real

    vector%x(:,:,:) = real
    vector%y(:,:,:) = real
    vector%z(:,:,:) = real
  end subroutine assignment_real_to_vector
  
  subroutine boundary_condition_fluid(fluid, grid_div_range, peripheral, status)
    type(field__fluid_t), intent(inout) :: fluid
    type(grid__div_range_xyz_t), intent(in) :: grid_div_range
    type(rank__peripheral_xyz_t), intent(in) :: peripheral

    integer(SI) :: status(MPI_STATUS_SIZE)
    integer(SI) :: ierror

    call boundary_condition_scalar(fluid%pressure, grid_div_range, peripheral, status)
    call boundary_condition_scalar(fluid%density, grid_div_range, peripheral, status)
    call boundary_condition_vector(fluid%flux, grid_div_range, peripheral, status)
  end subroutine boundary_condition_fluid


  subroutine boundary_condition_scalar(scalar, grid_div_range, peripheral, status)
    real(DR), dimension(NX,NY,NZ), intent(inout) :: scalar
    type(grid__div_range_xyz_t), intent(in) :: grid_div_range
    type(rank__peripheral_xyz_t), intent(in) :: peripheral

    integer(SI) :: status(MPI_STATUS_SIZE)
    integer(SI) :: ierror
    
        
    call mpi_sendrecv(scalar(grid_div_range%range_x%end_idx-1,:,:), NY*NZ,MPI_REAL8, peripheral%x%upper, 100,  &
                        scalar(grid_div_range%range_x%start_idx,:,:), NY*NZ,MPI_REAL8, peripheral%x%lower, 100, &
                        MPI_COMM_WORLD, status, ierror)

    call mpi_sendrecv(scalar(grid_div_range%range_x%start_idx+1,:,:), NY*NZ, MPI_REAL8, peripheral%x%lower, 100,  &
                        scalar(grid_div_range%range_x%end_idx,:,:), NY*NZ, MPI_REAL8, peripheral%x%upper, 100, &
                        MPI_COMM_WORLD, status, ierror)
   
    call mpi_sendrecv(scalar(:,grid_div_range%range_y%end_idx-1,:), NX*NZ,MPI_REAL8, peripheral%y%upper, 100,  &
                        scalar(:,grid_div_range%range_y%start_idx,:), NX*NZ,MPI_REAL8, peripheral%y%lower, 100, &
                        MPI_COMM_WORLD, status, ierror)
 
    call mpi_sendrecv(scalar(:,grid_div_range%range_y%start_idx+1,:), NX*NZ, MPI_REAL8, peripheral%y%lower, 100,  &
                        scalar(:,grid_div_range%range_y%end_idx,:), NX*NZ, MPI_REAL8, peripheral%y%upper, 100, &
                        MPI_COMM_WORLD, status, ierror)
  
    
    call mpi_sendrecv(scalar(:,:,grid_div_range%range_z%end_idx-1), NX*NY,MPI_REAL8, peripheral%z%upper, 100,  &
                        scalar(:,:,grid_div_range%range_z%start_idx), NX*NY,MPI_REAL8, peripheral%z%lower, 100, &
                        MPI_COMM_WORLD, status, ierror)

    call mpi_sendrecv(scalar(:,:,grid_div_range%range_z%start_idx+1), NX*NY, MPI_REAL8, peripheral%z%lower, 100,  &
                        scalar(:,:,grid_div_range%range_z%end_idx), NX*NY, MPI_REAL8, peripheral%z%upper, 100, &
                        MPI_COMM_WORLD, status, ierror)
    

    !scalar( 1,:,:) = scalar(NX-1,:,:)
    !scalar(NX,:,:) = scalar(   2,:,:)

    !scalar(:, 1,:) = scalar(:,NY-1,:)
    !scalar(:,NY,:) = scalar(:,   2,:)

    !scalar(:,:, 1) = scalar(:,:,NZ-1)
    !scalar(:,:,NZ) = scalar(:,:,   2)
  end subroutine boundary_condition_scalar


  subroutine boundary_condition_vector(vec, grid_div_range, peripheral, status)
    type(field__vector3d_t), intent(inout) :: vec
    type(rank__peripheral_xyz_t), intent(in) :: peripheral
    type(grid__div_range_xyz_t), intent(in) :: grid_div_range

    integer(SI) :: status(MPI_STATUS_SIZE)
    integer(SI) :: ierror
    integer(SI) :: i,j

    ! xz plane
    call mpi_sendrecv(vec%x(:,grid_div_range%range_y%end_idx-1,:), NX*NZ, MPI_REAL8, peripheral%y%upper, 100,  &
                        vec%x(:,grid_div_range%range_y%start_idx,:) ,NX*NZ, MPI_REAL8, peripheral%y%lower, 100, &
                        MPI_COMM_WORLD, status, ierror)

    call mpi_sendrecv(vec%y(:,grid_div_range%range_y%end_idx-1,:), NX*NZ, MPI_REAL8, peripheral%y%upper, 100,  &
                        vec%y(:,grid_div_range%range_y%start_idx,:) ,NX*NZ, MPI_REAL8, peripheral%y%lower, 100, &
                        MPI_COMM_WORLD, status, ierror)
    
    call mpi_sendrecv(vec%z(:,grid_div_range%range_y%end_idx-1,:), NX*NZ, MPI_REAL8, peripheral%y%upper, 100,  &
                        vec%z(:,grid_div_range%range_y%start_idx,:) ,NX*NZ, MPI_REAL8, peripheral%y%lower, 100, &
                        MPI_COMM_WORLD, status, ierror)  
    
    call mpi_sendrecv(vec%x(:,grid_div_range%range_y%start_idx+1,:), NX*NZ, MPI_REAL8, peripheral%y%lower, 100,  &
                        vec%x(:,grid_div_range%range_y%end_idx,:) ,NX*NZ, MPI_REAL8, peripheral%y%upper, 100, &
                        MPI_COMM_WORLD, status, ierror)
 
    call mpi_sendrecv(vec%y(:,grid_div_range%range_y%start_idx+1,:), NX*NZ, MPI_REAL8, peripheral%y%lower, 100,  &
                        vec%y(:,grid_div_range%range_y%end_idx,:) ,NX*NZ, MPI_REAL8, peripheral%y%upper, 100, &
                        MPI_COMM_WORLD, status, ierror)
    
    call mpi_sendrecv(vec%z(:,grid_div_range%range_y%start_idx+1,:), NX*NZ, MPI_REAL8, peripheral%y%lower, 100,  &
                        vec%z(:,grid_div_range%range_y%end_idx,:) ,NX*NZ, MPI_REAL8, peripheral%y%upper, 100, &
                        MPI_COMM_WORLD, status, ierror)
   
     
    !xy plane
    call mpi_sendrecv(vec%x(:,:,grid_div_range%range_z%end_idx-1), NX*NY, MPI_REAL8, peripheral%z%upper, 100,  &
                        vec%x(:,:,grid_div_range%range_z%start_idx) ,NX*NY, MPI_REAL8, peripheral%z%lower, 100, &
                        MPI_COMM_WORLD, status, ierror)
    
    call mpi_sendrecv(vec%y(:,:,grid_div_range%range_z%end_idx-1), NX*NY, MPI_REAL8, peripheral%z%upper, 100,  &
                        vec%y(:,:,grid_div_range%range_z%start_idx) ,NX*NY, MPI_REAL8, peripheral%z%lower, 100, &
                        MPI_COMM_WORLD, status, ierror)
    
    call mpi_sendrecv(vec%z(:,:,grid_div_range%range_z%end_idx-1), NX*NY, MPI_REAL8, peripheral%z%upper, 100,  &
                        vec%z(:,:,grid_div_range%range_z%start_idx) ,NX*NY, MPI_REAL8, peripheral%z%lower, 100, &
                        MPI_COMM_WORLD, status, ierror)
    
    call mpi_sendrecv(vec%x(:,:,grid_div_range%range_z%start_idx+1), NX*NY, MPI_REAL8, peripheral%z%lower, 100,  &
                        vec%x(:,:,grid_div_range%range_z%end_idx) ,NX*NY, MPI_REAL8, peripheral%z%upper, 100, &
                        MPI_COMM_WORLD, status, ierror)
 
    call mpi_sendrecv(vec%y(:,:,grid_div_range%range_z%start_idx+1), NX*NY, MPI_REAL8, peripheral%z%lower, 100,  &
                        vec%y(:,:,grid_div_range%range_z%end_idx) ,NX*NY, MPI_REAL8, peripheral%z%upper, 100, &
                        MPI_COMM_WORLD, status, ierror)
    
    call mpi_sendrecv(vec%z(:,:,grid_div_range%range_z%start_idx+1), NX*NY, MPI_REAL8, peripheral%z%lower, 100,  &
                        vec%z(:,:,grid_div_range%range_z%end_idx) ,NX*NY, MPI_REAL8, peripheral%z%upper, 100, &
                        MPI_COMM_WORLD, status, ierror)
     

    !yz plane
    call mpi_sendrecv(vec%x(grid_div_range%range_x%end_idx-1,:,:), NY*NZ, MPI_REAL8, peripheral%x%upper, 100,  &
                        vec%x(grid_div_range%range_x%start_idx,:,:) ,NY*NZ, MPI_REAL8, peripheral%x%lower, 100, &
                        MPI_COMM_WORLD, status, ierror)
    
    call mpi_sendrecv(vec%y(grid_div_range%range_x%end_idx-1,:,:), NY*NZ, MPI_REAL8, peripheral%x%upper, 150,  &
                        vec%y(grid_div_range%range_x%start_idx,:,:) ,NY*NZ, MPI_REAL8, peripheral%x%lower, 150, &
                        MPI_COMM_WORLD, status, ierror)  
    
    call mpi_sendrecv(vec%z(grid_div_range%range_x%end_idx-1,:,:), NY*NZ, MPI_REAL8, peripheral%x%upper, 100,  &
                        vec%z(grid_div_range%range_x%start_idx,:,:) ,NY*NZ, MPI_REAL8, peripheral%x%lower, 100, &
                        MPI_COMM_WORLD, status, ierror)
    
    call mpi_sendrecv(vec%x(grid_div_range%range_x%start_idx+1,:,:), NY*NZ, MPI_REAL8, peripheral%x%lower, 150,  &
                        vec%x(grid_div_range%range_x%end_idx,:,:) ,NY*NZ, MPI_REAL8, peripheral%x%upper, 150, &
                        MPI_COMM_WORLD, status, ierror)
    
    call mpi_sendrecv(vec%y(grid_div_range%range_x%start_idx+1,:,:), NY*NZ, MPI_REAL8, peripheral%x%lower, 150,  &
                        vec%y(grid_div_range%range_x%end_idx,:,:) ,NY*NZ, MPI_REAL8, peripheral%x%upper, 150, &
                        MPI_COMM_WORLD, status, ierror)
    
    call mpi_sendrecv(vec%z(grid_div_range%range_x%start_idx+1,:,:), NY*NZ, MPI_REAL8, peripheral%x%lower, 100,  &
                        vec%z(grid_div_range%range_x%end_idx,:,:) ,NY*NZ, MPI_REAL8, peripheral%x%upper, 100, &
                        MPI_COMM_WORLD, status, ierror)
    
 
  end subroutine boundary_condition_vector


  function operator_cross_product(a,b)
    type(field__vector3d_t), intent(in) :: a, b
    type(field__vector3d_t) :: operator_cross_product

    operator_cross_product%x = (a%y)*(b%z) - (a%z)*(b%y)
    operator_cross_product%y = (a%z)*(b%x) - (a%x)*(b%z)
    operator_cross_product%z = (a%x)*(b%y) - (a%y)*(b%x)
  end function operator_cross_product


  subroutine operator_curl(res, a, grid_div_range, peripheral , status)
    type(field__vector3d_t), intent(in) :: a
    type(field__vector3d_t), intent(out) :: res
    
    type(rank__peripheral_xyz_t), intent(in) :: peripheral
    type(grid__div_range_xyz_t), intent(in) :: grid_div_range

    integer(SI) :: status(MPI_STATUS_SIZE)
    integer(SI) :: ierror

    integer(SI) :: i, j, k
    real(DR) :: dx1, dy1, dz1

    dx1 = grid%d1%x
    dy1 = grid%d1%y
    dz1 = grid%d1%z

    do k = grid_div_range%range_z%start_idx+1 , grid_div_range%range_z%end_idx-1
      do j = grid_div_range%range_y%start_idx+1, grid_div_range%range_y%end_idx-1
       do i = grid_div_range%range_x%start_idx+1, grid_div_range%range_x%end_idx-1 
          res%x(i,j,k) = dy1*(a%z(i,j+1,k)-a%z(i,j-1,k)) &
                                 - dz1*(a%y(i,j,k+1)-a%y(i,j,k-1))
          res%y(i,j,k) = dz1*(a%x(i,j,k+1)-a%x(i,j,k-1)) &
                                 - dx1*(a%z(i+1,j,k)-a%z(i-1,j,k))
          res%z(i,j,k) = dx1*(a%y(i+1,j,k)-a%y(i-1,j,k)) &
                                 - dy1*(a%x(i,j+1,k)-a%x(i,j-1,k))
        end do
      end do
    end do

    call boundary_condition_vector(res, grid_div_range, peripheral, status)
  end subroutine operator_curl


  function operator_div(a, grid_div_range, peripheral, status)
    type(field__vector3d_t), intent(in)  :: a
    real(DR), dimension(NX,NY,NZ)        :: operator_div
    
    type(grid__div_range_xyz_t), intent(in) :: grid_div_range
    type(rank__peripheral_xyz_t), intent(in) :: peripheral

    integer(SI)  :: status(MPI_STATUS_SIZE)
    integer(SI) :: ierror

    integer(SI) :: i, j, k
    real(DR) :: dx1, dy1, dz1

    dx1 = grid%d1%x
    dy1 = grid%d1%y
    dz1 = grid%d1%z

    do k = grid_div_range%range_z%start_idx+1 , grid_div_range%range_z%end_idx-1
      do j = grid_div_range%range_y%start_idx+1, grid_div_range%range_y%end_idx-1
       do i = grid_div_range%range_x%start_idx+1, grid_div_range%range_x%end_idx-1 
           operator_div(i,j,k) = dx1*(a%x(i+1,j,k)-a%x(i-1,j,k)) &
                               + dy1*(a%y(i,j+1,k)-a%y(i,j-1,k)) &
                               + dz1*(a%z(i,j,k+1)-a%z(i,j,k-1))
         end do
      end do
    end do

    call boundary_condition_scalar(operator_div, grid_div_range, peripheral, status)
  end function operator_div


  function operator_dot_product(a,b)
    type(field__vector3d_t), intent(in) :: a, b
    real(DR), dimension(NX,NY,NZ) :: operator_dot_product

    operator_dot_product = a%x*b%x +a%y*b%y + a%z*b%z
  end function operator_dot_product


  function operator_energyintegral(a, grid_div_range)
    type(field__fluid_t), intent(in) :: a
    type(grid__div_range_xyz_t), intent(in) :: grid_div_range
    real(DR)                         :: operator_energyintegral
    !
    !   flow_energy = (1/2) * rho * vel^2 = (1/2) * (massflux)^2 / rho
    !
    real(DR) :: dvol
    real(DR), dimension(NX,NY,NZ) :: flux_sq

    dvol = (grid%delta%x)*(grid%delta%y)*(grid%delta%z)
         !  Here we suppose that the grid spacings are uniform.
         !_______________________________________________________/

!   flux_sq = (a%flux).dot.(a%flux)
    flux_sq = operator_dot_product(a%flux,a%flux)

    operator_energyintegral                                      &
         = 0.5_DR * sum(    flux_sq(grid_div_range%range_x%start_idx+1: &
           grid_div_range%range_x%end_idx-1, &
           grid_div_range%range_y%start_idx+1: & 
           grid_div_range%range_y%end_idx-1, & 
           grid_div_range%range_z%start_idx+1: & 
           grid_div_range%range_z%end_idx-1) &
            / a%density(grid_div_range%range_x%start_idx+1: &
           grid_div_range%range_x%end_idx-1, &
           grid_div_range%range_y%start_idx+1: & 
           grid_div_range%range_y%end_idx-1, & 
           grid_div_range%range_z%start_idx+1: & 
           grid_div_range%range_z%end_idx-1 )) * dvol
  end function operator_energyintegral


  function operator_fluid_add(a,b)
    type(field__fluid_t), intent(in) :: a
    type(field__fluid_t), intent(in) :: b
    type(field__fluid_t) :: operator_fluid_add

    operator_fluid_add%flux%x   = a%flux%x   + b%flux%x
    operator_fluid_add%flux%y   = a%flux%y   + b%flux%y
    operator_fluid_add%flux%z   = a%flux%z   + b%flux%z
    operator_fluid_add%density  = a%density  + b%density
    operator_fluid_add%pressure = a%pressure + b%pressure
  end function operator_fluid_add
  
  function operator_fluid_times_integer(fluid,integer)
    type(field__fluid_t), intent(in) :: fluid
    integer(SI),          intent(in) :: integer
    type(field__fluid_t) :: operator_fluid_times_integer

    operator_fluid_times_integer%pressure = integer*(fluid%pressure)
    operator_fluid_times_integer%density  = integer*(fluid%density)
    operator_fluid_times_integer%flux%x   = integer*(fluid%flux%x)
    operator_fluid_times_integer%flux%y   = integer*(fluid%flux%y)
    operator_fluid_times_integer%flux%z   = integer*(fluid%flux%z)
  end function operator_fluid_times_integer


  function operator_fluid_times_real(fluid,real)
    type(field__fluid_t), intent(in) :: fluid
    real(DR),             intent(in) :: real
    type(field__fluid_t) :: operator_fluid_times_real

    operator_fluid_times_real%pressure = real*(fluid%pressure)
    operator_fluid_times_real%density  = real*(fluid%density)
    operator_fluid_times_real%flux%x   = real*(fluid%flux%x)
    operator_fluid_times_real%flux%y   = real*(fluid%flux%y)
    operator_fluid_times_real%flux%z   = real*(fluid%flux%z)
  end function operator_fluid_times_real


  function operator_integer_times_fluid(integer,fluid)
    integer(SI),          intent(in) :: integer
    type(field__fluid_t), intent(in) :: fluid
    type(field__fluid_t) :: operator_integer_times_fluid

    operator_integer_times_fluid%pressure = integer*(fluid%pressure)
    operator_integer_times_fluid%density  = integer*(fluid%density)
    operator_integer_times_fluid%flux%x   = integer*(fluid%flux%x)
    operator_integer_times_fluid%flux%y   = integer*(fluid%flux%y)
    operator_integer_times_fluid%flux%z   = integer*(fluid%flux%z)
  end function operator_integer_times_fluid


  function operator_laplacian_scalar(a, grid_div_range, peripheral, status)
    real(DR), dimension(NX,NY,NZ), intent(in) :: a
    real(DR), dimension(NX,NY,NZ) :: operator_laplacian_scalar
  
    type(grid__div_range_xyz_t), intent(in) :: grid_div_range
    type(rank__peripheral_xyz_t), intent(in) :: peripheral

    integer(SI) :: status(MPI_STATUS_SIZE)
    integer(SI) :: ierror
  
    integer(SI) :: i, j, k
    real(DR) :: dx2, dy2, dz2

    dx2 = grid%d2%x
    dy2 = grid%d2%y
    dz2 = grid%d2%z

    do k = grid_div_range%range_z%start_idx+1 , grid_div_range%range_z%end_idx-1
      do j = grid_div_range%range_y%start_idx+1, grid_div_range%range_y%end_idx-1
       do i = grid_div_range%range_x%start_idx+1, grid_div_range%range_x%end_idx-1 
           operator_laplacian_scalar(i,j,k)  &
               = dx2*(a(i+1,j,k)-2*a(i,j,k)+a(i-1,j,k))  &
               + dy2*(a(i,j+1,k)-2*a(i,j,k)+a(i,j-1,k))  &
               + dz2*(a(i,j,k+1)-2*a(i,j,k)+a(i,j,k-1))
        end do
      end do
    end do

    call boundary_condition_scalar(operator_laplacian_scalar, grid_div_range, peripheral, status)
  end function operator_laplacian_scalar


  function operator_laplacian_vector(a, grid_div_range, peripheral, status)
    type(field__vector3d_t), intent(in) :: a
    type(field__vector3d_t) :: operator_laplacian_vector

    type(grid__div_range_xyz_t), intent(in) :: grid_div_range
    type(rank__peripheral_xyz_t), intent(in) :: peripheral

    integer(SI) :: status(MPI_STATUS_SIZE)

    integer(SI) :: i, j, k
    real(DR) :: dx2, dy2, dz2

    dx2 = grid%d2%x
    dy2 = grid%d2%y
    dz2 = grid%d2%z

    do k = grid_div_range%range_z%start_idx+1 , grid_div_range%range_z%end_idx-1
      do j = grid_div_range%range_y%start_idx+1, grid_div_range%range_y%end_idx-1
       do i = grid_div_range%range_x%start_idx+1, grid_div_range%range_x%end_idx-1 
          operator_laplacian_vector%x(i,j,k)  &
               = dx2*(a%x(i+1,j,k)-2*a%x(i,j,k)+a%x(i-1,j,k))  &
               + dy2*(a%x(i,j+1,k)-2*a%x(i,j,k)+a%x(i,j-1,k))  &
               + dz2*(a%x(i,j,k+1)-2*a%x(i,j,k)+a%x(i,j,k-1))
          operator_laplacian_vector%y(i,j,k)  &
               = dx2*(a%y(i+1,j,k)-2*a%y(i,j,k)+a%y(i-1,j,k))  &
               + dy2*(a%y(i,j+1,k)-2*a%y(i,j,k)+a%y(i,j-1,k))  &
               + dz2*(a%y(i,j,k+1)-2*a%y(i,j,k)+a%y(i,j,k-1))
          operator_laplacian_vector%z(i,j,k)  &
               = dx2*(a%z(i+1,j,k)-2*a%z(i,j,k)+a%z(i-1,j,k))  &
               + dy2*(a%z(i,j+1,k)-2*a%z(i,j,k)+a%z(i,j-1,k))  &
               + dz2*(a%z(i,j,k+1)-2*a%z(i,j,k)+a%z(i,j,k-1))
        end do
     end do
    end do

    call boundary_condition_vector(operator_laplacian_vector,grid_div_range,peripheral,status)
  end function operator_laplacian_vector


  function operator_real_times_fluid(real,fluid)
    real(DR),             intent(in) :: real
    type(field__fluid_t), intent(in) :: fluid
    type(field__fluid_t) :: operator_real_times_fluid

    operator_real_times_fluid%pressure = real*(fluid%pressure)
    operator_real_times_fluid%density  = real*(fluid%density)
    operator_real_times_fluid%flux%x   = real*(fluid%flux%x)
    operator_real_times_fluid%flux%y   = real*(fluid%flux%y)
    operator_real_times_fluid%flux%z   = real*(fluid%flux%z)
  end function operator_real_times_fluid


  function operator_real_times_vector(real,vec)
    real(DR),                intent(in) :: real
    type(field__vector3d_t), intent(in) :: vec
    type(field__vector3d_t) :: operator_real_times_vector

    operator_real_times_vector%x = real*(vec%x)
    operator_real_times_vector%y = real*(vec%y)
    operator_real_times_vector%z = real*(vec%z)
  end function operator_real_times_vector


  function operator_scalar_times_vector(scalar,vec)
    real(DR), dimension(NX,NY,NZ), intent(in) :: scalar
    type(field__vector3d_t),        intent(in) :: vec
    type(field__vector3d_t)             :: operator_scalar_times_vector

    operator_scalar_times_vector%x = scalar*(vec%x)
    operator_scalar_times_vector%y = scalar*(vec%y)
    operator_scalar_times_vector%z = scalar*(vec%z)
  end function operator_scalar_times_vector


  function operator_scalarintegral(a, grid_div_range)
    real(DR), dimension(NX,NY,NZ), intent(in) :: a
    type(grid__div_range_xyz_t), intent(in) :: grid_div_range
    real(DR) :: operator_scalarintegral

    real(DR) :: dvol

    dvol = (grid%delta%x)*(grid%delta%y)*(grid%delta%z)
         !
         !  Here we suppose that the grid spacings are uniform.
         !
    operator_scalarintegral = sum( a(grid_div_range%range_x%start_idx+1: &
           grid_div_range%range_x%end_idx-1, &
           grid_div_range%range_y%start_idx+1: & 
           grid_div_range%range_y%end_idx-1, & 
           grid_div_range%range_z%start_idx+1: & 
           grid_div_range%range_z%end_idx-1) ) * dvol
  end function operator_scalarintegral


  function operator_vector_add(a,b)
    type(field__vector3d_t), intent(in) :: a, b
    type(field__vector3d_t) :: operator_vector_add

    operator_vector_add%x = a%x + b%x
    operator_vector_add%y = a%y + b%y
    operator_vector_add%z = a%z + b%z
  end function operator_vector_add


  function operator_vector_divby_scalar(vec,scalar)
    type(field__vector3d_t),       intent(in) :: vec
    real(DR), dimension(NX,NY,NZ), intent(in) :: scalar
    type(field__vector3d_t) :: operator_vector_divby_scalar
    
    operator_vector_divby_scalar%x = (vec%x) / scalar
    operator_vector_divby_scalar%y = (vec%y) / scalar
    operator_vector_divby_scalar%z = (vec%z) / scalar
  end function operator_vector_divby_scalar


  function operator_vector_times_real(vec,real)
    type(field__vector3d_t), intent(in) :: vec
    real(DR),                intent(in) :: real
    type(field__vector3d_t) :: operator_vector_times_real

    operator_vector_times_real%x = real*(vec%x)
    operator_vector_times_real%y = real*(vec%y)
    operator_vector_times_real%z = real*(vec%z)
  end function operator_vector_times_real


  function operator_vector_times_scalar(vec,scalar)
    type(field__vector3d_t),       intent(in) :: vec
    real(DR), dimension(NX,NY,NZ), intent(in) :: scalar
    type(field__vector3d_t) :: operator_vector_times_scalar

    operator_vector_times_scalar%x = scalar*(vec%x)
    operator_vector_times_scalar%y = scalar*(vec%y)
    operator_vector_times_scalar%z = scalar*(vec%z)
  end function operator_vector_times_scalar

end module field_m
