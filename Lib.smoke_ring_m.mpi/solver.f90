!!>
!   author: Akira Kageyama
!   date: 2023.05.05
! 
!   ナビエ・ストークス方程式ソルバ
! 
!   @note
!     Initialize_done, Viscosity など、頭文字だけが大文字の
!     変数名はこのモジュール全体にscopeを持つ変数である。
!     つまりこのモジュールの中の任意のサブルーチン・関数からアクセスできる。
!     ただし、このモジュールの外からはアクセスできない（privateである）
! 
!   @note
!     サブルーチンsubfield_velでは構造体の割り算
!        vel = fluid.flux / fluid.density 
!     をコメントアウトとしている。
!     Fortranコンパイラが自己定義演算子を問題なく使えるのであれば
!     コメントを外してこれを使うほうが記述が簡潔になる。
!     詳しくはfieldモジュールを見よ。
!      
!     サブルーチンsolver__advanceでの
!         gluid = fluid + dfluid01*0.5_DR
!     などといった記述についても同様。
! 
!   @bug
!     関数 solver__set_time_step で
!        vmax = maxval(sqrt(vel.x**2+vel.y**2+vel.z**2))
!     という演算はmaxvalとsqrtは交換して
!        vmax = sqrt(maxval(vel.x**2+vel.y**2+vel.z**2))
!     とした方が速くなる可能性が高い。
!     このままだと3次元配列の全要素にsqrt
!     を掛けてからそのmaxvalをとっているが、この順番は
!     逆にした方が速いだろう。
! 
!    @bug
!     その少し下の
!        sound_v = GAMMA*maxval(sqrt(GASS_CONST_FOR_AIR*tm)) 
!     も同様に
!        sound_v = GAMMA*sqrt(maxval(GASS_CONST_FOR_AIR*tm)) 
!     とすべきだろう。
!!< 

module solver_m
  use constants_m  !! 定数定義
  use field_m      !! スカラー場・ベクトル場
  use fluid_m      !! 流体場の構造体定義
  use grid_m       !! 格子点
  use kutimer_m    !! 時間測定モジュール
  use mpiut_m      !! MPIユーティリティ
  use parallel_m   !! MPI並列化
  use params_m     !! パラメータ
  use ut_m         !! ユーティリティ 
  implicit none    !! 暗黙の型宣言無効化。必須

  private !! このモジュール内の変数・ルーチン等はデフォルトで非公開

  type, private :: solver_t
  contains
    procedure, nopass :: advance => solver__advance
    procedure, nopass :: initialize => solver__initialize
    procedure, nopass :: set_time_step => solver__set_time_step
    procedure, nopass, private :: subfield_vel
    procedure, nopass, private :: subfield_vel_tm
    procedure, nopass, private :: subfield_vel_tm_divv
    generic :: get_subfield => subfield_vel,  &
                               subfield_vel_tm,  &
                               subfield_vel_tm_divv
    !! 流体の基本変数（質量フラックス、質量密度、圧力）
    !! から二次的な量（流れ場、温度場、速度の発散）
    !! を計算するためのルーチン群の多重定義
  end type solver_t

  type(solver_t), public :: Solver


  real(DR), parameter :: GAMMA = 1.4_DR                 !! 空気の比熱比
  real(DR), parameter :: GASS_CONST_FOR_AIR = 2.87e2_DR !! 空気の気体定数
    !!  空気の状態方程式
    !!     Pressure = 287 * Mass_density * Temperature
  real(DR), save :: Viscous_diffusivity   !! 空気の粘性（速度拡散率）
  real(DR), save :: Thermal_diffusivity   !! 空気の温度拡散係数
  real(DR), save :: Gamma1_kappa_factor        
    !!            = (GAMMA-1) * kappa / rho
    !!  圧力の時間発展方程式で使う係数。
    !!     \partial p / \partial t = ... + (GAMMA-1)*kappa*\nabla^2 T
    !!                             = rho*Gamma1_kappa_factor*\nabla^2 T

  type(field__vector_t), save :: Drive_force   !! 渦輪を駆動する力

  logical,  save :: Initialize_done = .false.  !! モジュール初期化確認フラグ


contains


  function drive_force_factor( time )
    !! 渦輪を駆動する力の時間変化の調整のための係数設定
    !!
    !! @note この係数は0以上1以下。これはassertで確認している。
    !!
    real(DR), intent(in) :: time !! シミュレーション時刻

    real(DR) :: drive_force_factor  !! 力の強さ係数 0から1
    !!>
!                                           factor
!                   ___________               |
!                  /|         |\              |
!           ______/ |         | \______       +--------> time
!                 | |         |  |
!                 | |         |  |
!                 | t0        t1 |
!               t_start         t_end
    !!<
    real(DR), parameter :: T_START =  0.0_DR  !! 力をかけ始める時刻
    ! real(DR), parameter :: T_END   =  0.01_DR !! 力をかけ終わる時刻
    real(DR), parameter :: T_END   =  0.1_DR !! 力をかけ終わる時刻
                                           !! 試行錯誤で調整せよ
    real(DR), parameter :: T0 = T_START + (T_END-T_START)/4
    real(DR), parameter :: T1 = T_END   - (T_END-T_START)/4
                                           !! 上のコメント図をみよ
                                           !! これも試行錯誤で調整せよ

    real(DR), parameter :: ONE  = 1.0_DR  !! コードの読みやすさのため定義
    real(DR), parameter :: ZERO = 0.0_DR  !! コードの読みやすさのため定義

    logical :: just_once = .true.

    if (just_once) then 
      call iPrint
    just_once = .false. ; end if 

    call ut__assert( T_START < T0 .and. T0 < T1 .and. T1 < T_END,  &
                    "solver_m(133): Time inconsistent.")
      !! この関係は以下の前提なのでアサートで確認しておく

    !! 上のコメント図の時間依存係数
    if ( time <= T_START ) then
      drive_force_factor = ZERO
    else if ( time <= T0 ) then
      drive_force_factor = (time-T_START) / (T0-T_START)
    else if ( time <= T1 ) then
      drive_force_factor = ONE
    else if ( time <= T_END ) then
      drive_force_factor = ONE - (time-T1) / (T_END-T1)
    else
      drive_force_factor = ZERO
    end if

    call ut__assert( drive_force_factor >=0.0_DR .and.  &
                     drive_force_factor <=1.0_DR,       &
                     "solver_m(151): strange value." )

  contains

    subroutine iPrint
      integer :: comm
      comm = Parallel%comm
      call mpiut__message_leader( comm, "drive_force_factor: T_START",  &
                                         T_START )
      call mpiut__message_leader( comm, "drive_force_factor: T_END",  &
                                         T_END )
    end subroutine iPrint

  end function drive_force_factor


  subroutine set_drive_force_field
    !! 渦輪を駆動するための力の場を設定する
    !! その力はシミュレーション開始直後、短い時間だけかける。
    !! 空間的には局在した力を想定している。その形状は円筒形である。
    !! 円筒の軸はx軸上にある。
    integer  :: i, j, k
    real(DR) :: xx, yy, zz
    real(DR) :: force_region_x_min, force_region_x_max
    real(DR) :: force_center_y, force_center_z
    real(DR) :: force_cylinder_diameter
    real(DR) :: force_cylinder_radius_outer
    real(DR) :: force_cylinder_radius_inner
    real(DR) :: force_cylinder_radius_buffer
    real(DR) :: rr, radial_factor

    real(DR), parameter :: THE_FORCE = 1.e2_DR
                                     !! 瞬間的な力の最大値。
                                     !! 試行錯誤で調整せよ。
    !!>
!          +--------------------------------------+ ZMAX
!          |                                      |
!          |    +-------+                         |
!          |    | Force |                         |
!          |    +-------+                         |
!          |                                      |
!          +--------------------------------------+ ZMIN
!         XMIN                                   XMAX
    !!<
    force_region_x_min = XMIN + (XMAX-XMIN)/5
      !! 力をかける局所円筒領域のx方向の最小値
    force_region_x_max = force_region_x_min + (XMAX-XMIN)/5
      !! 力をかける局所円筒領域のx方向の最大値
    force_center_y = (YMAX + YMIN) / 2
      !! 力をかける局所円筒領域の中心のy座標を中間にとる
    force_center_z = (ZMAX + ZMIN) / 2
      !! 力をかける局所円筒領域の中心のz座標を中間にとる
    force_cylinder_diameter  = min(YMAX-YMIN, ZMAX-ZMIN) / 4
      !! 力をかける局所円筒領域の直径
    force_cylinder_radius_outer = force_cylinder_diameter / 2
      !! 力をかける局所円筒領域の半径
    force_cylinder_radius_buffer = force_cylinder_radius_outer * 0.2_DR
    force_cylinder_radius_inner  = force_cylinder_radius_outer  &
                                 - force_cylinder_radius_buffer
      !!      ___radius_outer               ___radius_inner
      !!     /   ___radius_inner           /   ___radius_outer
      !!    /   /                         /   /
      !!   +---+------------O------------+---+
      !!   |___|                         |___|
      !!     \___radius_buffer             \___radius_buffer
      !!  

    do k = 1, NZPP
      !! 境界上の格子点を飛ばして、シミュレーション領域内部
      !! を回る3重do loop。境界上の格子点で値は境界条件で設定する。
      zz = grid%pos%z(k) - force_center_z    !! 力の中心からのz方向の距離
      do j = 1, NYPP
        yy = grid%pos%y(j) - force_center_y  !! 力の中心からのy方向の距離
        rr = sqrt(yy**2+zz**2)               !! 力の中心軸からの距離

        if ( rr <= force_cylinder_radius_inner ) then
          radial_factor = 1.0_DR
        else if ( rr <= force_cylinder_radius_outer ) then
          radial_factor = ( force_cylinder_radius_outer - rr )  &
                          / force_cylinder_radius_buffer
        else 
          radial_factor = 0.0_DR
        end if

        do i = 1, NXPP
          xx = grid%pos%x(i)                 !! これは格子点のx座標そのもの
          if ( (xx > force_region_x_min)  &
                        .and.  &
               (xx < force_region_x_max) ) then
            !! ここで半径そのもので比較するとsqrtの計算が必要になるが
            !! このように2乗同士で比較すれば不要。
            Drive_force%x(i,j,k) = radial_factor * THE_FORCE 
                                              !! いまはx方向だけに力がかかる
            Drive_force%y(i,j,k) = 0.0_DR     !! としている。斜め方向に力を
            Drive_force%z(i,j,k) = 0.0_DR     !! かけるにはこの部分を変更。
          else
            Drive_force%x(i,j,k) = 0.0_DR
            Drive_force%y(i,j,k) = 0.0_DR
            Drive_force%z(i,j,k) = 0.0_DR
          end if
        end do
      end do
    end do

    call field__boundary_condition( Drive_force )
      !! 1. プロセス間通信を行い、分割領域の境界値を設定する
      !! 2. シミュレーション領域全体の境界条件を設定する。

    call iPrint

  contains

    subroutine iPrint
      integer :: comm
      comm = Parallel%comm
      call mpiut__message_leader( comm, "THE_FORCE",  &
                                         THE_FORCE )
    end subroutine iPrint

  end subroutine set_drive_force_field


  subroutine subfield_vel( fluid, vel )
    !! fluidの基本変数から二次的な場vel（流れの速度ベクトル場）
    !! をもとめる。
    type(fluid_t) , intent(in)  :: fluid  !! 流体基本場
    type(field__vector_t), intent(out) :: vel    !! 流れの速度ベクトル

    vel = fluid%flux / fluid%density
      !! 割り算演算子はfield_mで定義している。
  end subroutine subfield_vel


  subroutine subfield_vel_tm( fluid, vel, tm )
    !! fluidの基本変数から二次的な場（速度ベクトル場velと温度場tm）
    !! をもとめる。
    type(fluid_t) , intent(in)  :: fluid !! 流体基本場
    type(field__vector_t), intent(out) :: vel   !! 流れ場
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(out) :: tm    !! 温度場

    vel = fluid%flux / fluid%density 
     tm = fluid%pressure / (GASS_CONST_FOR_AIR*fluid%density)
      !! fluid構造体の割り算演算子はfluid_mで定義している。
  end subroutine subfield_vel_tm


  subroutine subfield_vel_tm_divv( fluid, vel, tm, divv )
    !! fluidの基本変数から二次的な場（速度ベクトル場velと
    !! 温度場tmと速度の発散divv）をもとめる。
    type(fluid_t) , intent(in)  :: fluid  !! 流体基本場
    type(field__vector_t), intent(out) :: vel    !! 流れ場
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(out) :: tm     !! 温度場
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(out) :: divv   !! 流れの発散

    vel = fluid%flux     / fluid%density ! operator defined in field.f90.
     tm = fluid%pressure / fluid%density
    divv = .div. vel
  end subroutine subfield_vel_tm_divv


  function the_equation( t, dt, vx, vy, vz, tm, divv, fx, fy, fz, ps, rho )
    !! ナビエ・ストークス方程式の右辺（時間変化量）dtを掛けたものを計算
    !!
    !! @note
    !!    粘性による加熱の効果は小さいので無視している。
    !!    この項を入れる場合は圧力の方程式
    !!    the_equation.pressure に追加すればよい。
    real(DR), intent(in) :: t, dt      !! 時刻と時間刻み幅
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(in) :: vx, vy, vz !! 速度3成分
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(in) :: tm, divv   !! 温度と速度の発散
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(in) :: fx, fy, fz !! 質量フラックス
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(in) :: ps         !! 圧力
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(in) :: rho        !! 質量密度
    type(fluid_t) :: the_equation     !! 時間刻みdtでの流体データの微小変化量

    integer  :: i, j, k
    real(DR), parameter :: ONE_THIRD = 1.0_DR / 3.0_DR  !! 演算回数節約のため
    real(DR) :: gradpx, gradpy, gradpz     !! 圧力の勾配 (gradient p)
    real(DR) :: gdivvx, gdivvy, gdivvz     !! 速度の発散の勾配
    real(DR) :: divfvx, divfvy, divfvz     !! 速度・密度フラックステンソルの発散
    real(DR) :: lapvx, lapvy, lapvz, laptm !! 速度と温度のラプラシアン
    real(DR) :: divf                       !! 質量フラックスの発散
    real(DR) :: factor                     !! 渦輪駆動力の係数（時間依存）
    real(DR) :: viscosity                  !! 粘性係数
    real(DR) :: gamma1_kappa               !! 熱拡散による圧力上昇係数

    call ut__assert( Initialize_done,   &
                     "solver_m(338): Forgot init?" )
      !! 初期化忘れしていないか確認

    factor = drive_force_factor( t )
      !! 渦輪の駆動力はシミュレーション開始直後だけかける。
      !! その後は何も力をかけない（渦輪が自然に発生し、リング上の
      !! 構造が移動していく。）つまりこのfactorはシミュレーション
      !! 開始直後だけ非ゼロで、残りの殆どの時間はゼロが入っている。

    !! 以下のdo loopがこのシミュレーションで最も時間のかかる
    !! 部分である。したがってここでは.div.などのユーザ定義
    !! 演算子は（コンパイラがその使用を許したとしても）あえて
    !! 使わず、泥臭く書いている。これは将来、
    !! 速度向上のためにコードの最適化をしたり、
    !! OpenMP化することを見越してのことである。
    do k = 1, NZPP
      do j = 1, NYPP
        do i = 1, NXPP
          !! 以下のコメントでは  
          !! P 圧力
          !! V 速度場ベクトル
          !! F 質量フラックスベクトル
          !! T 温度

          !! grad P
          gradpx = ( ps(i+1,j,k)-ps(i-1,j,k) ) * grid%d1%x
          gradpy = ( ps(i,j+1,k)-ps(i,j-1,k) ) * grid%d1%y
          gradpz = ( ps(i,j,k+1)-ps(i,j,k-1) ) * grid%d1%z

          !! grad (div V) の3成分
          gdivvx = ( divv(i+1,j,k)-divv(i-1,j,k) ) * grid%d1%x
          gdivvy = ( divv(i,j+1,k)-divv(i,j-1,k) ) * grid%d1%y
          gdivvz = ( divv(i,j,k+1)-divv(i,j,k-1) ) * grid%d1%z

          !! VFテンソルの発散 div(VF) の3成分
          divfvx = ( fx(i+1,j,k)*vx(i+1,j,k)  &
                    -fx(i-1,j,k)*vx(i-1,j,k) ) * grid%d1%x  &
                 + ( fx(i,j+1,k)*vy(i,j+1,k)  &
                    -fx(i,j-1,k)*vy(i,j-1,k) ) * grid%d1%y  &
                 + ( fx(i,j,k+1)*vz(i,j,k+1)  &
                    -fx(i,j,k-1)*vz(i,j,k-1) ) * grid%d1%z
          divfvy = ( fy(i+1,j,k)*vx(i+1,j,k)  &
                    -fy(i-1,j,k)*vx(i-1,j,k) ) * grid%d1%x  &
                 + ( fy(i,j+1,k)*vy(i,j+1,k)  &
                    -fy(i,j-1,k)*vy(i,j-1,k) ) * grid%d1%y  &
                 + ( fy(i,j,k+1)*vz(i,j,k+1)  &
                    -fy(i,j,k-1)*vz(i,j,k-1) ) * grid%d1%z
          divfvz = ( fz(i+1,j,k)*vx(i+1,j,k)  &
                    -fz(i-1,j,k)*vx(i-1,j,k) ) * grid%d1%x  &
                 + ( fz(i,j+1,k)*vy(i,j+1,k)  &
                    -fz(i,j-1,k)*vy(i,j-1,k) ) * grid%d1%y  &
                 + ( fz(i,j,k+1)*vz(i,j,k+1)  &
                    -fz(i,j,k-1)*vz(i,j,k-1) ) * grid%d1%z

          !! Laplacin V の3成分
          lapvx = ( vx(i+1,j,k)-2*vx(i,j,k)+vx(i-1,j,k) )*grid%d2%x &
                + ( vx(i,j+1,k)-2*vx(i,j,k)+vx(i,j-1,k) )*grid%d2%y &
                + ( vx(i,j,k+1)-2*vx(i,j,k)+vx(i,j,k-1) )*grid%d2%z
          lapvy = ( vy(i+1,j,k)-2*vy(i,j,k)+vy(i-1,j,k) )*grid%d2%x &
                + ( vy(i,j+1,k)-2*vy(i,j,k)+vy(i,j-1,k) )*grid%d2%y &
                + ( vy(i,j,k+1)-2*vy(i,j,k)+vy(i,j,k-1) )*grid%d2%z
          lapvz = ( vz(i+1,j,k)-2*vz(i,j,k)+vz(i-1,j,k) )*grid%d2%x &
                + ( vz(i,j+1,k)-2*vz(i,j,k)+vz(i,j-1,k) )*grid%d2%y &
                + ( vz(i,j,k+1)-2*vz(i,j,k)+vz(i,j,k-1) )*grid%d2%z

          !! Laplacin T 
          laptm = ( tm(i+1,j,k)-2*tm(i,j,k)+tm(i-1,j,k) )*grid%d2%x &
                + ( tm(i,j+1,k)-2*tm(i,j,k)+tm(i,j-1,k) )*grid%d2%y &
                + ( tm(i,j,k+1)-2*tm(i,j,k)+tm(i,j,k-1) )*grid%d2%z

          !! div F 
          divf = ( fx(i+1,j,k)-fx(i-1,j,k) ) * grid%d1%x  &
               + ( fy(i,j+1,k)-fy(i,j-1,k) ) * grid%d1%y  &
               + ( fz(i,j,k+1)-fz(i,j,k-1) ) * grid%d1%z

          !! 以下がナビエ・ストークス方程式

          !! はじめに係数を準備しておく
          viscosity = rho(i,j,k) * Viscous_diffusivity
          gamma1_kappa = rho(i,j,k) * Gamma1_kappa_factor

          !!  密度の時間発展
          the_equation%density(i,j,k) = -divf*dt

          !!  質量フラックスの時間発展（3成分）

          the_equation%flux%x(i,j,k) =  &
               ( - divfvx  &
                 - gradpx  &
                 + Drive_force%x(i,j,k)*factor  &
                 + viscosity * ( lapvx + ONE_THIRD*gdivvx )  &
               ) * dt
          the_equation%flux%y(i,j,k) =  &
               ( - divfvy  &
                 - gradpy  &
                 + Drive_force%y(i,j,k)*factor  &
                 + viscosity * ( lapvy + ONE_THIRD*gdivvy )  &
               ) * dt
          the_equation%flux%z(i,j,k) =  &
               ( - divfvz  &
                 - gradpz  &
                 + Drive_force%z(i,j,k)*factor  &
                 + viscosity * ( lapvz + ONE_THIRD*gdivvz )  &
               ) * dt

          !!  圧力の時間発展
          the_equation%pressure(i,j,k) =  &
              ( - ( vx(i,j,k)*gradpx  &
                  + vy(i,j,k)*gradpy  &
                  + vz(i,j,k)*gradpz  &
                  )  &
                + gamma1_kappa * laptm  &
                - GAMMA * ps(i,j,k) * divv(i,j,k)  &
              ) * dt
        end do
      end do
    end do

    call the_equation%set_boundary_condition
      !! 境界条件の設定（周期境界条件）
      !! 上のdo loopは境界面上の格子点を除いた格子点（シミュレーション
      !! 領域の内部の格子点）上での値を計算するものであった。
      !! ここで境界面上の格子点の値の（更新されたばかりの）内部の
      !! 格子点上のデータをコピーすることで設定する。
  end function the_equation


!
! Private  これより上が非公開の関数・ルーチン
!----------------------------------------------- 
! Public   これより下が公開する関数・ルーチン
!


  subroutine solver__advance( t, dt, fluid )
    !! 4段4次ルンゲ・クッタ積分法による時間積分の実行
    !!
    !! @note
    !!   ここでは教科書に書かれている古典的な4段4次の
    !!   ルンゲ・クッタ積分法をそのまま実装している。
    !!   作業用の構造体を5つ使用している（dfluid01からdfluid04とgluid）
    !!   これらの作業変数の数を減らし、メモリを節約にするためには
    !!   Runge-Kutta-Gill法などの方法がある。
    !!
    !! @note
    !!   速度（vel）、温度（tm）, 速度の発散（divv）などの配列などは
    !!   このスキームでは基本変数から毎回計算すべき一種の作業配列である。
    !!   したがって、このモジュール内の他の場所（サブルーチン・関数）でも
    !!   これらの変数（3次元の大きな配列）を宣言・使用している。
    !!   使用メモリを節約するためにはこれらの作業配列は共通のものを
    !!   一つづつ用意するというのも可能である。しかし、そうするとコードが
    !!   読みにくくなるであろう。

    real(DR), intent(inout) :: t  !! 時刻
    real(DR), intent(in) :: dt !! 時間刻み幅
    type(fluid_t), intent(inout)  :: fluid !! 流体データ

    real(DR), parameter :: ONE_SIXTH = 1.0_DR / 6.0_DR !! 演算数節約の
    real(DR), parameter :: ONE_THIRD = 1.0_DR / 3.0_DR !! ために定義

    type(field__vector_t) :: vel   !! 速度場
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1) :: tm    !! 温度場
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1) :: divv  !! 速度場の発散

    type(fluid_t) :: dfluid01, dfluid02, dfluid03, dfluid04
      !! 古典的な4段4次ルンゲ・クッタ積分法に必要な4つの作業配列
      !! サイズが大きいことに注意。メモリに余裕があるという前提。
    type(fluid_t) :: gluid  !! work variable
      !! さらにもう一つの作業配列。サイズが大きい。

                                                         call kutimer__start('solv a')
    !!---ルンゲ・クッタの第1段---
    call subfield_vel_tm_divv( fluid, vel, tm, divv )   ;call kutimer__('solv a','vltmdv')
      !! 基本変数から副次的変数である速度、温度、速度の発散を求める
    dfluid01 = the_equation( t, dt,  &
                             vel%x, vel%y, vel%z, tm, divv,  &
                             fluid%flux%x, fluid%flux%y, fluid%flux%z,  &
                             fluid%pressure, fluid%density )
                                                         call kutimer__('solv a','equtin')
    t = t + dt/2
      !! 渦輪を駆動する力は時刻（t）の関数として設定しているので、
      !! いま解いているナビエ・ストークス方程式は時間に陽に依存する。

    !!---ルンゲ・クッタの第2段---
    gluid = fluid + dfluid01*0.5_DR                     ;call kutimer__('solv a','g=f+df')
      !! 演算子定義を活用しているので記述は簡潔だが計算量は多い。

    call subfield_vel_tm_divv( gluid, vel, tm, divv )   ;call kutimer__('solv a','vltmdv')
    dfluid02 = the_equation( t, dt,  &
                             vel%x, vel%y, vel%z, tm, divv,  &
                             gluid%flux%x, gluid%flux%y, gluid%flux%z,  &
                             gluid%pressure, gluid%density )
                                                         call kutimer__('solv a','equtin')
    !!---ルンゲ・クッタの第3段---
    gluid = fluid + dfluid02*0.5_DR                     ;call kutimer__('solv a','g=f+df')
      !! 演算子定義を活用しているので記述は簡潔だが計算量は多い。

    call subfield_vel_tm_divv( gluid, vel, tm, divv )   ;call kutimer__('solv a','vltmdv')
    dfluid03 = the_equation( t, dt,  &
                             vel%x, vel%y, vel%z, tm, divv,  &
                             gluid%flux%x, gluid%flux%y, gluid%flux%z,  &
                             gluid%pressure, gluid%density )
                                                         call kutimer__('solv a','equtin')
    t = t + dt/2
      !! 渦輪を駆動する力は時刻（t）の関数として設定しているので、
      !! いま解いているナビエ・ストークス方程式は時間に陽に依存する。

    !!---ルンゲ・クッタの第4段---
    gluid = fluid + dfluid03                            ;call kutimer__('solv a','g=f+df')
      !! 演算子定義を活用しているので記述は簡潔だが計算量は多い。

    call subfield_vel_tm_divv( gluid, vel, tm, divv )   ;call kutimer__('solv a','vltmdv')
    dfluid04 = the_equation( t, dt,  &
                             vel%x, vel%y, vel%z, tm, divv,  &
                             gluid%flux%x, gluid%flux%y, gluid%flux%z,  &
                             gluid%pressure, gluid%density )
                                                         call kutimer__('solv a','equtin')
    !!--- 最終結果 ---
    fluid = fluid + ONE_SIXTH*( dfluid01 + 2*dfluid02 + 2*dfluid03 + dfluid04 )
                                                         call kutimer__('solv a','f=f+df')
      !! 演算子定義を活用しているので記述は簡潔だが計算量は多い。
                                                         call kutimer__end('solv a')
  end subroutine solver__advance


  subroutine solver__initialize( fluid )
    !! モジュールの初期化
    type(fluid_t), intent(out) :: fluid  !! 流体データ

    !! 物理パラメータの設定
    Viscous_diffusivity = params%get_double( 'Viscous_diffusivity' ) 
      !! 空気速度の拡散率

    Thermal_diffusivity = params%get_double( 'Thermal_diffusivity' ) 
      !! 空気温度の拡散率
      !!>
!         ここは混乱しやすい。
!
!         熱エネルギーフラックスを
!           (1)    q_h = -kappa \nabla T
!         単位質量あたりのエントロピーを
!           (2)    s = c_p log T - (k_B/m_p) log p
!         とする。熱力学より
!           (3)    rho T (ds/dt) = kappa \nabla^2 T
!         である。質量保存則と状態方程式
!           (4)    p = rho (k_B/m_p) T
!         より圧力の時間発展方程式 
!           (5)    dp/dt = -gamma p \nabla \cdot v + (gamma-1) kappa \nabla^2 T
!         と温度の時間発展方程式
!           (6)    c_v rho (dT/dt) = -p \nabla \cdot v + kappa \nabla^2 T
!         を得る。ここで
!           (7)    c_v = 1 / (gamma-1) * (k_B/m_p)
!         は単位質量あたりの定積比熱である。（m_pは粒子質量。）
!
!         ここで、式(5)と(6)の拡散項に注目すると、
!           (8)    \partial p / \partial t = ... + kappa * (gamma-1) \nabla^2 T
!           (9)    \partial T / \partial t = ... + kappa / (c_v rho) \nabla^2 T
!         である。式(9)の係数を熱拡散係数（温度の拡散係数）
!           (10)   k = kappa / (c_v rho)
!         と呼ぶことにすると、k と kappa の関係は
!           (11)   kappa = rho * c_v * k
!         である。(7)より
!           (12)   kappa * (gamma-1) = rho * (k_B/m_p) * k
!         いまこのコードでは
!           (13)   k_B/m_p = GASS_CONST_FOR_AIR
!         と定義しているので、
!           (14)  Gamma1_kappa = (gamma-1)*kappa 
!                              = rho * GASS_CONST_FOR_AIR * k
!         である。
      !!<
    Gamma1_kappa_factor = GASS_CONST_FOR_AIR * Thermal_diffusivity
      !! 先頭が大文字のモジュールグローバル変数

    !! 流体の初期条件の設定
    fluid%pressure = 1.013e5_DR  !! 1013 hPa (一気圧)
    fluid%density  = 1.293_DR    !! kg/m^3 (空気の密度)
    fluid%flux     = 0.0_DR      !! ベクトル3成分。（静止状態）

    !! 渦輪を駆動するための力の場の設定
    call set_drive_force_field

    Initialize_done = .true.
      !! モジュール初期化終了フラグ

    call iPrint

  contains

    subroutine iPrint
      integer :: comm
      comm = Parallel%comm
      call mpiut__message_leader( comm, "GAMMA",                &
                                         GAMMA )
      call mpiut__message_leader( comm, "GASS_CONST_FOR_AIR",   &
                                         GASS_CONST_FOR_AIR )
      call mpiut__message_leader( comm, "Gamma1_kappa_factor",  &
                                         Gamma1_kappa_factor )
      call mpiut__message_leader( comm, "Initial pressure",     &
                                         fluid%pressure(1,1,1) )
      call mpiut__message_leader( comm, "Initial density",      &
                                         fluid%density(1,1,1) )
      call mpiut__message_leader( comm, "Initial fx",           &
                                         fluid%flux%x(1,1,1) )
      call mpiut__message_leader( comm, "Initial fy",           &
                                         fluid%flux%y(1,1,1) )
      call mpiut__message_leader( comm, "Initial fz",           &
                                         fluid%flux%z(1,1,1) )
    end subroutine iPrint
  end subroutine solver__initialize


  function solver__set_time_step( nloop, fluid )
    !! CFL条件に基づいて時間刻み幅dtを設定する
    integer, intent(in) :: nloop  !! ループカウンタ
    type(fluid_t), intent(in) :: fluid  !! 流体データ
    real(DR) :: solver__set_time_step !! 時間刻み幅 dt

    type(field__vector_t) :: vel !! 速度場
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1) :: tm  !! 温度場

    real(DR) :: vmax, sound_v 
      !! 流れ速度の最大値と音速
    real(DR) :: dt_vel, dt_sound, dt_viscous, dt_kappa
      !! 流れ速度、音波、粘性拡散、熱拡散、それぞれで決まる時間刻み幅
      !! 実際のdtはこれらのなかで最も厳しい（小さい）値できまる。
    real(DR), parameter :: ALMOST_ZERO = 1.e-20_DR 
      !! ゼロ割り演算回避のために使う小さい値
    real(DR), parameter :: ANOMALOUS_VALUE = -999.999_DR 
      !! dtとしてありそうにない値（すぐ下で使う）
    real(DR) :: dt_local  
      !! 各MPIプロセスで計算した時間刻み幅
    real(DR), save :: dt_global = ANOMALOUS_VALUE
      !! 時間刻み幅。全プロセスの最小値。
      !! 初期設定でなんらかの失敗した場合を検出するため
      !! 最初はありそうない値を設定しておくが、
      !! CFL条件に基づいて正しい値を一度設定したらその後は
      !! この宣言文にはsave属性がついているので、
      !! この関数を抜けたあともその値を覚えている。
    integer, parameter :: SKIP = 20
      !! dtを毎ステップ計算するのは大変（かなりの時間がかかる）
      !! のでこのSKIPステップに一度だけ計算する。毎ステップ計算する
      !! してもdtは1ステップでそれほど大きく変化しないからである。
      !! ただし、計算が破綻（発散）するような場合は例外である。
    integer :: ctr = 0 !! スキップ用のループカウンタ


    call ut__assert( Initialize_done,  &
                     "solver_m(685): Forgot init?" )
      !! 初期化忘れ確認

    if(mod(ctr,SKIP)==0) then ! ほとんどの場合は、前回計算したdtを使う。
      call subfield_vel_tm(fluid,vel,tm)
        !! 基本流体データから速度場と温度場を計算

      vmax = maxval(sqrt(vel%x**2+vel%y**2+vel%z**2))
        !! 速度の最大値（ベクトルの長さの最大値）をもとめる
        !! ここでは配列演算を駆使して1行で書いているが
        !! 実際にはかなりの演算をしていることに注意。
        !! これはバグではないが、maxvalとsqrtは交換すべき
        !! だろう。このままだと3次元配列の全要素にsqrt
        !! を掛けてからそのmaxvalをとっているが、これを
        !! 逆にした方が速いかもしれない。

      vmax = max(vmax,ALMOST_ZERO) 
        !! 初期条件では速度場がゼロなのでvmax=0となるが、あとで
        !! vmaxの割り算が出てくるので問題となる。それを回避するため
        !! ALMOST_ZEROが十分小さければよい。

      sound_v = GAMMA*maxval(sqrt(GASS_CONST_FOR_AIR*tm)) 
        !! 音速の最大値
        !! これも上と同じ理屈で、maxvalとsqrtは交換すべき
        !! だろう。このままだと3次元配列の全要素にsqrt
        !! を掛けてからそのmaxvalをとっているが、これを
        !! 逆にした方が速いかもしれない。

      call ut__assert( sound_v > ALMOST_ZERO,  &
                       "solver_m(714): sound_v=0?" )
        !! 音速（の最大値）がほとんどゼロになるのは
        !! 何かがおかしいのですぐに停止

      !! 以下では、流れの速さ、音波、粘性拡散、熱拡散の4種類の
      !! CFL条件で決まる時間刻み幅をそれぞれこの順番に求めている。
      dt_vel     = 0.8_DR*grid%delta_min/vmax
      dt_sound   = 0.8_DR*grid%delta_min/sound_v
      dt_viscous = 0.2_DR*(grid%delta_min**2)/Viscous_diffusivity
      dt_kappa   = 0.2_DR*(grid%delta_min**2)/Thermal_diffusivity
        !! CFL factor はここでは0.8と0.2にしているが、
        !! これは精密な議論に基づいて決めたものではなく、
        !! 半経験的に決めた値である。
        !! 最後の2つ、粘性拡散と熱拡散によるCFL条件のための
        !! CFL factor の値 (0.2) というのは少々安全側に設定
        !! しすぎているかもしれない。つまりもう少し大きくしても
        !! 問題ないかもしれない。

      dt_local = min( dt_vel, dt_sound, dt_viscous, dt_kappa )
        !! 最終的な時間刻み幅は上記の4種類のdtの最小値できまる。

      call mpiut__minval( Parallel%comm, dt_local, dt_global )
        !! すべてのプロセスでの最小値をとる

      if ( mod(nloop,SKIP*100)==0 ) then
        !! 出力が長くなるが、流体の状態を推測するのに便利なデータ。
        !! SKIPをさらに10倍しているのは、それほど頻繁に出力する
        !! 必要は通常ないからである。
        associate ( comm => Parallel%comm )
          call mpiut__message_leader( comm, '     nloop', nloop      )
          call mpiut__message_leader( comm, '      vmax', vmax       )
          call mpiut__message_leader( comm, '    dt_vel', dt_vel     )
          call mpiut__message_leader( comm, '  dt_sound', dt_sound   )
          call mpiut__message_leader( comm, '  dt_kappa', dt_kappa   )
          call mpiut__message_leader( comm, 'dt_viscous', dt_viscous )
          call mpiut__message_leader( comm, '    --> dt', dt_global  )
        end associate
      end if
    end if; ctr = ctr + 1 

    call ut__assert( dt_global /= ANOMALOUS_VALUE,  &
                     "solver_m(755): dt init failed?" )
      !! SKIPに一度dtを計算し直すが、それ以外は下の行を見れば分かる通り
      !! 前回のdtの値を流用する。その際、一度もdtを計算したことがなかったら
      !! まずいのでその検出をANOMALOUS_VALUEを使って検出している。

    solver__set_time_step = dt_global   !! dt of the prev calc is saved.
      !! 新たに更新（または前回計算した）dtを返す。
  end function solver__set_time_step

end module solver_m
