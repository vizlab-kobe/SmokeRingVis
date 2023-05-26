!!>
!     author: Akira Kageyama
!     date: 2023.05.05
!   
!     Fortran定数と数学定数
!   
!     @note 定数であることが目で見てわかりやすいように全ての文字を
!           大文字で書いているがコンパイラには無意味。
!     
!     @note シミュレーション領域は直方体と仮定している。
!           x方向の長さXMAX-XMINである。座標系の原点を
!           シミュレーション領域の中心におくためには
!           XMAXとXMIN絶対値を等しくとる。
!     
!     @note シミュレーションの（x方向の）空間解像度を上げる
!           ためには同じXMAX-XMINに対して格子点数NXを上げれば良い。
!     
!     @note x,y,z それぞれの方向の空間解像度、
!           つまり格子間隔dx,dy,dzが異なっていても構わないが、
!           その差が極端に大きくしないほうがよい。
!!< 
module constants_m
  use iso_fortran_env
  implicit none

  logical, parameter :: DEBUG_MODE = .true.

  ! << f90 constants >>
  integer, parameter :: SI = int32     ! 単精度整数
  integer, parameter :: DI = int64     ! 倍精度整数
  integer, parameter :: SR = real32    ! 単精度実数
  integer, parameter :: DR = real64    ! 倍精度実数

  ! << Mathematical constants >>
  real(DR), parameter :: PI = atan(1.0_DR)*4
  real(DR), parameter :: TWOPI = PI*2

  !<< Code utility constants >>!
  integer, parameter :: NIL = -huge(1)
  integer, parameter :: STR_LEN_MAX = 200

  !<< MPI process constants >>!
!  integer, parameter :: NPROC_X = 8 ! x方向MPIプロセス数 mkjob.sh でこの行をgrepする
!  integer, parameter :: NPROC_Y = 4 ! y方向MPIプロセス数 mkjob.sh でこの行をgrepする 
!  integer, parameter :: NPROC_Z = 4 ! z方向MPIプロセス数 mkjob.sh でこの行をgrepする
  !! Test 1 (Small: 2 x 1 x 1 = 2 procs.)
  !! {
  integer, parameter :: NPROC_X = 2 ! x方向MPIプロセス数 mkjob.sh でこの行をgrepする
  integer, parameter :: NPROC_Y = 1 ! y方向MPIプロセス数 mkjob.sh でこの行をgrepする 
  integer, parameter :: NPROC_Z = 1 ! z方向MPIプロセス数 mkjob.sh でこの行をgrepする
  !! }

  !<< Grid size constants >>!
!  integer, parameter :: NXPP =  20   ! PP = Per Process 
!  integer, parameter :: NYPP =  20
!  integer, parameter :: NZPP =  20
  !! Test 1 (Small: 2 x 1 x 1 = 2 procs.)
  !! {
  integer, parameter :: NXPP =  40   ! PP = Per Process 
  integer, parameter :: NYPP =  40
  integer, parameter :: NZPP =  40
  !! }
  integer, parameter :: NXPP1 = NXPP + 1  ! PP1 = PP plus one
  integer, parameter :: NYPP1 = NYPP + 1
  integer, parameter :: NZPP1 = NZPP + 1
  integer, parameter :: NX_GLOBAL = NXPP * NPROC_X + 2 ! x方向の全格子点数
  integer, parameter :: NY_GLOBAL = NYPP * NPROC_Y + 2
  integer, parameter :: NZ_GLOBAL = NZPP * NPROC_Z + 2

  ! 格子サイズのメモ
  ! NX_GLOBAL = 162  ! Vis=Therm=8e-3 → 1.4万で発散。1e-2 → 20万までOK
  ! NY_GLOBAL =  82  ! THE_FORCE 1e2 ==> vmax = 5 m/s
  ! NZ_GLOBAL =  82  ! longer force cylinder   128 MPI procs 
  ! 
  ! NX_GLOBAL = 242  ! Vis=Therm=5e-3: 発散 5万ステップ
  ! NY_GLOBAL = 122  ! 
  ! NZ_GLOBAL = 122  ! 

  ! << Box Size >>
  real(DR), parameter :: XMIN = -10.0_DR ! 計算領域範囲 +x  単位はメートル
  real(DR), parameter :: XMAX = +10.0_DR ! 計算領域範囲 -x
  real(DR), parameter :: YMIN =  -5.0_DR ! 計算領域範囲 +y
  real(DR), parameter :: YMAX =  +5.0_DR ! 計算領域範囲 -y
  real(DR), parameter :: ZMIN =  -5.0_DR ! 計算領域範囲 +z
  real(DR), parameter :: ZMAX =  +5.0_DR ! 計算領域範囲 -z
end module constants_m
