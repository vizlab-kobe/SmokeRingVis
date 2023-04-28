!-------------------------------------------------------------------
! class-hpc-smoke-ring: A simple sample field solver.
!
!    by Akira Kageyama, Kobe University, Japan.
!       email: sgks@mac.com
!
!    This software is released under the MIT License.
!
!-------------------------------------------------------------------
!    src/constants.f90
!-------------------------------------------------------------------

module constants_m
  implicit none

  ! << f90 constants >>
  integer, parameter :: SI = selected_int_kind(8)
  integer, parameter :: DI = selected_int_kind(16)
  integer, parameter :: SR = selected_real_kind(6)
  integer, parameter :: DR = selected_real_kind(12)

  ! << Mathematical constants >>
  real(DR), parameter :: PI = 3.1415926535897932_DR
  real(DR), parameter :: TWOPI = PI*2

  ! << Grid Size >>
  integer(SI), parameter :: NX =  90
  integer(SI), parameter :: NY =  30
  integer(SI), parameter :: NZ =  30
 
  ! integer(SI), parameter :: NX =  60
  ! integer(SI), parameter :: NY =  20
  ! integer(SI), parameter :: NZ =  20
 
  ! integer(SI), parameter :: NX = 156
  ! integer(SI), parameter :: NY = 52
  ! integer(SI), parameter :: NZ = 52

  ! fix nprocs as 3 * 2 * 2 = 12
  integer(SI), parameter :: X_NPROCS = 3
  integer(SI), parameter :: Y_NPROCS = 2
  integer(SI), parameter :: Z_NPROCS = 2
  
  integer(SI), parameter :: NX_DIV = NX / X_NPROCS !52 
  integer(SI), parameter :: NY_DIV = NY / Y_NPROCS !26
  integer(SI), parameter :: NZ_DIV = NZ / Z_NPROCS !26

  ! << Box Size >>
  real(DR), parameter :: XMIN = -1.5_DR
  real(DR), parameter :: XMAX = +1.5_DR
  real(DR), parameter :: YMIN = -0.5_DR
  real(DR), parameter :: YMAX = +0.5_DR
  real(DR), parameter :: ZMIN = -0.5_DR
  real(DR), parameter :: ZMAX = +0.5_DR
end module constants_m
