!*******************************************************************
!> author: Akira Kageyama
!  license: MIT
!  date: 2020.01.22
!
!  ナビエ・ストークス方程式ソルバ
!
!  @note
!    Initialize_done, Viscosity など、頭文字だけが大文字の
!    変数名はこのモジュール全体にscopeを持つ変数である。
!    つまりこのモジュールの中の任意のサブルーチン・関数からアクセスできる。
!    ただし、このモジュールの外からはアクセスできない（privateである）
!
!  @note
!    サブルーチンsubfield_velでは構造体の割り算
!       vel = fluid%flux / fluid%density 
!    をコメントアウトとしている。
!    Fortranコンパイラが自己定義演算子を問題なく使えるのであれば
!    コメントを外してこれを使うほうが記述が簡潔になる。
!    詳しくはfieldモジュールを見よ。
!     
!    サブルーチンsolver__advanceでの
!        gluid = fluid + dfluid01*0.5_DR
!    などといった記述についても同様。
!
! @bug
!    サブルーチン solver__diagnosis の発散判定で
!        if ( maxval(fluid%flux%x) > ABNORMALLY_LARGE ) then
!    としているところ、正しくは
!        if ( maxval(abs(fluid%flux%x)) > ABNORMALLY_LARGE ) then
!    とすべき。このままだと-x方向に大きな値があるときに引っかからない。
! 
!  @bug
!    関数 solver__set_time_step で
!       vmax = maxval(sqrt(vel%x**2+vel%y**2+vel%z**2))
!    という演算はmaxvalとsqrtは交換して
!       vmax = sqrt(maxval(vel%x**2+vel%y**2+vel%z**2))
!    とした方が速くなる可能性が高い。
!    このままだと3次元配列の全要素にsqrt
!    を掛けてからそのmaxvalをとっているが、この順番は
!    逆にした方が速いだろう。
!
!   @bug
!    その少し下の
!       sound_v = GAMMA*maxval(sqrt(GASS_CONST_FOR_AIR*tm)) 
!    も同様に
!       sound_v = GAMMA*sqrt(maxval(GASS_CONST_FOR_AIR*tm)) 
!    とすべきだろう。
!

module solver_m
  use constants_m  ! 定数定義
  use grid_m       ! 格子点
  use ut_m         ! ユーティリティ 
  use params_m     ! パラメータ
  use field_m      ! 流れ場データ構造体
  use debug_m      ! デバッグ用
  use job_m        ! ジョブ管理
  implicit none    ! 暗黙の型宣言無効化。必須

  private ! このモジュール内の変数・ルーチン等はデフォルトで非公開
  public :: solver__advance,  &
            solver__diagnosis,  &
            solver__get_subfield,  &
            solver__initialize,  &
            solver__set_time_step

  interface solver__get_subfield
    !! 流体の基本変数（質量フラックス、質量密度、圧力）
    !! から二次的な量（流れ場、温度場、速度の発散）
    !! を計算するためのルーチン群の多重定義
    module procedure subfield_vel,  &
                     subfield_vel_tm,  &
                     subfield_vel_tm_divv
  end interface

  real(DR), parameter :: GAMMA = 1.4_DR                 ! 空気の比熱比
  real(DR), parameter :: GASS_CONST_FOR_AIR = 2.87e2_DR ! 空気の気体定数
    !  空気の状態方程式
    !     Pressure = 287 * Mass_density * Temperature

  logical,  save :: Initialize_done = .false.  ! モジュール初期化確認フラグ
  real(DR), save :: Viscosity                  ! 粘性率
  real(DR), save :: Gamma1_kappa               ! (GAMMA-1)*kappa
                                               ! kappaは熱拡散率
  type(field__vector3d_t), save :: Drive_force ! 渦輪を駆動する力


contains


  function drive_force_factor(time)
    !! 渦輪を駆動する力の時間変化の調整のための係数設定
    !!
    !! @note この係数は0以上1以下。これはassertで確認している。
    !!
    real(DR), intent(in) :: time !! シミュレーション時刻
    real(DR)             :: drive_force_factor  !! 力の強さ係数 0から1
    !
    !                                      factor
    !              ___________               |
    !             /|         |\              |
    !      ______/ |         | \______       +--------> time
    !            | |         |  |
    !            | |         |  |
    !            | t0        t1 |
    !          t_start         t_end
    !
    real(DR), parameter :: T_START =  0.0_DR  ! 力をかけ始める時刻
    real(DR), parameter :: T_END   =  0.01_DR ! 力をかけ終わる時刻
                                              ! 試行錯誤で調整せよ
    real(DR), parameter :: T0 = T_START + (T_END-T_START)/4
    real(DR), parameter :: T1 = T_END   - (T_END-T_START)/4
                                              ! 上のコメント図をみよ
                                              ! これも試行錯誤で調整せよ

    real(DR), parameter :: ONE  = 1.0_DR  ! コードの読みやすさのため定義
    real(DR), parameter :: ZERO = 0.0_DR  ! コードの読みやすさのため定義

    call ut__assert( T_START < T0 .and. T0 < T1 .and. T1 < T_END,  &
                    "<solver/drive_force_factor> Time inconsistent.")
      ! この関係は以下の前提なのでアサートで確認しておく

    ! 上のコメント図の時間依存係数
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

    call ut__assert(drive_force_factor >=0.0_DR  &
                              .and.  &
                    drive_force_factor <=1.0_DR,  &
                    "<solver/drive_force_factor> strange value.")
  end function drive_force_factor


  subroutine set_drive_force_field
    !! 渦輪を駆動するための力の場を設定する
    !! その力はシミュレーション開始直後、短い時間だけかける。
    !! 空間的には局在した力を想定している。その形状は円筒形である。
    !! 円筒の軸はx軸上にある。
    integer(SI) :: i, j, k
    real(DR) :: xx, yy, zz
    real(DR) :: force_region_x_min, force_region_x_max
    real(DR) :: force_center_y, force_center_z
    real(DR) :: force_cylinder_diameter, force_cylinder_radius_sq

    real(DR), parameter :: THE_FORCE = 3.e3_DR
                                     ! 瞬間的な力の最大値。
                                     ! 試行錯誤で調整せよ。
    !
    !     +--------------------------------------+ ZMAX
    !     |                                      |
    !     |    +-------+                         |
    !     |    | Force |                         |
    !     |    +-------+                         |
    !     |                                      |
    !     +--------------------------------------+ ZMIN
    !    XMIN                                   XMAX
    !
    force_region_x_min = XMIN + (XMAX-XMIN)/5
      ! 力をかける局所円筒領域のx方向の最小値
    force_region_x_max = force_region_x_min + (XMAX-XMIN)/10
      ! 力をかける局所円筒領域のx方向の最大値
    force_center_y = (YMAX + YMIN) / 2
      ! 力をかける局所円筒領域の中心のy座標を中間にとる
    force_center_z = (ZMAX + ZMIN) / 2
      ! 力をかける局所円筒領域の中心のz座標を中間にとる
    force_cylinder_diameter  = min(YMAX-YMIN, ZMAX-ZMIN) / 4
      ! 力をかける局所円筒領域の直径
    force_cylinder_radius_sq = (force_cylinder_diameter/2)**2
      ! 力をかける局所円筒領域の半径の2乗

    do k = 2 , NZ-1
      ! 境界上の格子点を飛ばして、シミュレーション領域内部
      ! を回る3重do loop。境界上の格子点で値は境界条件で設定する。
      zz = grid%pos%z(k) - force_center_z    ! 力の中心からのz方向の距離
      do j = 2 , NY-1
        yy = grid%pos%y(j) - force_center_y  ! 力の中心からのy方向の距離
        do i = 2 , NX-1
          xx = grid%pos%x(i)                 ! これは格子点のx座標そのもの
          if ( (yy**2+zz**2) < force_cylinder_radius_sq  &
                        .and.  &
               (xx > force_region_x_min)  &
                        .and.  &
               (xx < force_region_x_max) ) then
            ! ここで半径そのもので比較するとsqrtの計算が必要になるが
            ! このように2乗同士で比較すれば不要。
            Drive_force%x(i,j,k) = THE_FORCE  ! いまはx方向だけに力がかかる
            Drive_force%y(i,j,k) = 0.0_DR     ! としている。斜め方向に力を
            Drive_force%z(i,j,k) = 0.0_DR     ! かけるにはこの部分を変更。
          else
            Drive_force%x(i,j,k) = 0.0_DR
            Drive_force%y(i,j,k) = 0.0_DR
            Drive_force%z(i,j,k) = 0.0_DR
          end if
        end do
      end do
    end do

    call field__boundary_condition(Drive_force)
      ! 上の3重do loopで設定していない境界上の格子点に力の場を
      ! 設定する。周期境界条件。

    call ut__assert(maxval(Drive_force%x)==THE_FORCE,  &
                    "<solver/set_drive_force_field> something is wrong.")
      ! 最大値がTHE_FORCEと仮定しているのでそうでなければ何かがおかしい。

    call debug__print("called solver/set_drive_force_field.")
      ! デバッグモードがonの時には毎回メッセージを標準出力に
      ! 書き出す。コードが完成し、product runの段階に入ったら
      ! デバッグモードをoffにすれば、書き出しは抑制される。
      ! この行をコメントアウトする必要はない。
  end subroutine set_drive_force_field


  subroutine subfield_vel(fluid,vel)
    !! fluidの基本変数から二次的な場vel（流れの速度ベクトル場）
    !! をもとめる。
    type(field__fluid_t),    intent(in)  :: fluid  !! 流体基本場
    type(field__vector3d_t), intent(out) :: vel    !! 流れの速度ベクトル

![  vel = fluid%flux / fluid%density     ! operator defined in field.
    vel = operator_vector_divby_scalar(fluid%flux, fluid%density)
      ! Fortranコンパイラが自己定義演算子（field.f90で定義）
      ! を許せば上の簡潔な記述を使うべし。

    call debug__print("called solver/subfield_vel.")
      ! デバッグモードがonの時には毎回メッセージを標準出力に
      ! 書き出す。コードが完成し、product runの段階に入ったら
      ! デバッグモードをoffにすれば、書き出しは抑制される。
      ! この行をコメントアウトする必要はない。
  end subroutine subfield_vel


  subroutine subfield_vel_tm(fluid,vel,tm)
    !! fluidの基本変数から二次的な場（速度ベクトル場velと温度場tm）
    !! をもとめる。
    type(field__fluid_t),          intent(in)  :: fluid !! 流体基本場
    type(field__vector3d_t),       intent(out) :: vel   !! 流れ場
    real(DR), dimension(NX,NY,NZ), intent(out) :: tm    !! 温度場

![   vel = fluid%flux / fluid%density ! operator defined in field.f90.
     vel = operator_vector_divby_scalar(fluid%flux, fluid%density)
      tm = fluid%pressure / (GASS_CONST_FOR_AIR*fluid%density)
       ! Fortranコンパイラが自己定義演算子（field.f90で定義）
       ! を許せば上の簡潔な記述を使うべし。

    call debug__print("called solver/subfield_vel_tm.")
  end subroutine subfield_vel_tm


  subroutine subfield_vel_tm_divv(fluid,vel,tm,divv)
    !! fluidの基本変数から二次的な場（速度ベクトル場velと
    !! 温度場tmと速度の発散divv）をもとめる。
    type(field__fluid_t),          intent(in)  :: fluid  !! 流体基本場
    type(field__vector3d_t),       intent(out) :: vel    !! 流れ場
    real(DR), dimension(NX,NY,NZ), intent(out) :: tm     !! 温度場
    real(DR), dimension(NX,NY,NZ), intent(out) :: divv   !! 流れの発散

![   vel = fluid%flux     / fluid%density ! operator defined in field.f90.
     vel = operator_vector_divby_scalar(fluid%flux, fluid%density)
      tm = fluid%pressure / fluid%density
![  divv = .div.vel
    divv = operator_div(vel)
       ! Fortranコンパイラが自己定義演算子（field.f90で定義）
       ! を許せば上の簡潔な記述を使うべし。

    call debug__print("called solver/subfield_vel_tm_divv.")
  end subroutine subfield_vel_tm_divv


  function the_equation(t,dt,vx,vy,vz,tm,divv,fx,fy,fz,ps)
    !! ナビエ・ストークス方程式の右辺（時間変化量）dtを掛けたものを計算
    !!
    !! @note
    !!    粘性による加熱の効果は小さいので無視している。
    !!    この項を入れる場合は圧力の方程式
    !!    the_equation%pressure に追加すればよい。
    real(DR),                      intent(in) :: t, dt      !! 時刻と時間刻み幅
    real(DR), dimension(NX,NY,NZ), intent(in) :: vx, vy, vz !! 速度3成分
    real(DR), dimension(NX,NY,NZ), intent(in) :: tm, divv   !! 温度と速度の発散
    real(DR), dimension(NX,NY,NZ), intent(in) :: fx, fy, fz !! 質量フラックス
    real(DR), dimension(NX,NY,NZ), intent(in) :: ps         !! 圧力
    type(field__fluid_t) :: the_equation  !! 時間刻みdtでの流体データの微小変化量

    integer(SI) :: i, j, k
    real(DR), parameter :: ONE_THIRD = 1.0_DR / 3.0_DR  ! 演算回数節約のため
    real(DR) :: gradpx, gradpy, gradpz     ! 圧力の勾配 (gradient p)
    real(DR) :: gdivvx, gdivvy, gdivvz     ! 速度の発散の勾配
    real(DR) :: divfvx, divfvy, divfvz     ! 速度・密度フラックステンソルの発散
    real(DR) :: lapvx, lapvy, lapvz, laptm ! 速度と温度のラプラシアン
    real(DR) :: divf                       ! 質量フラックスの発散
    real(DR) :: factor                     ! 渦輪駆動力の係数（時間依存）

    call ut__assert(Initialize_done, "<solver/the_equation> Forgot init?")
      ! 初期化忘れしていないか確認

    factor = drive_force_factor(t)
      ! 渦輪の駆動力はシミュレーション開始直後だけかける。
      ! その後は何も力をかけない（渦輪が自然に発生し、リング上の
      ! 構造が移動していく。）つまりこのfactorはシミュレーション
      ! 開始直後だけ非ゼロで、残りの殆どの時間はゼロが入っている。

    ! 以下のdo loopがこのシミュレーションで最も時間のかかる
    ! 部分である。したがってここでは.div.などのユーザ定義
    ! 演算子は（コンパイラがその使用を許したとしても）あえて
    ! 使わず、泥臭く書いている。これは将来、
    ! 速度向上のためにコードの最適化をしたり、
    ! OpenMP化することを見越してのことである。
    do k = 2 , NZ-1
      do j = 2 , NY-1
        do i = 2 , NX-1
          ! 以下のコメントでは  
          ! P 圧力
          ! V 速度場ベクトル
          ! F 質量フラックスベクトル
          ! T 温度

          ! grad P
          gradpx = ( ps(i+1,j,k)-ps(i-1,j,k) ) * grid%d1%x
          gradpy = ( ps(i,j+1,k)-ps(i,j-1,k) ) * grid%d1%y
          gradpz = ( ps(i,j,k+1)-ps(i,j,k-1) ) * grid%d1%z

          ! grad (div V) の3成分
          gdivvx = ( divv(i+1,j,k)-divv(i-1,j,k) ) * grid%d1%x
          gdivvy = ( divv(i,j+1,k)-divv(i,j-1,k) ) * grid%d1%y
          gdivvz = ( divv(i,j,k+1)-divv(i,j,k-1) ) * grid%d1%z

          ! VFテンソルの発散 div(VF) の3成分
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

          ! Laplacin V の3成分
          lapvx = ( vx(i+1,j,k)-2*vx(i,j,k)+vx(i-1,j,k) )*grid%d2%x &
                + ( vx(i,j+1,k)-2*vx(i,j,k)+vx(i,j-1,k) )*grid%d2%y &
                + ( vx(i,j,k+1)-2*vx(i,j,k)+vx(i,j,k-1) )*grid%d2%z
          lapvy = ( vy(i+1,j,k)-2*vy(i,j,k)+vy(i-1,j,k) )*grid%d2%x &
                + ( vy(i,j+1,k)-2*vy(i,j,k)+vy(i,j-1,k) )*grid%d2%y &
                + ( vy(i,j,k+1)-2*vy(i,j,k)+vy(i,j,k-1) )*grid%d2%z
          lapvz = ( vz(i+1,j,k)-2*vz(i,j,k)+vz(i-1,j,k) )*grid%d2%x &
                + ( vz(i,j+1,k)-2*vz(i,j,k)+vz(i,j-1,k) )*grid%d2%y &
                + ( vz(i,j,k+1)-2*vz(i,j,k)+vz(i,j,k-1) )*grid%d2%z

          ! Laplacin T 
          laptm = ( tm(i+1,j,k)-2*tm(i,j,k)+tm(i-1,j,k) )*grid%d2%x &
                + ( tm(i,j+1,k)-2*tm(i,j,k)+tm(i,j-1,k) )*grid%d2%y &
                + ( tm(i,j,k+1)-2*tm(i,j,k)+tm(i,j,k-1) )*grid%d2%z

          ! div F 
          divf = ( fx(i+1,j,k)-fx(i-1,j,k) ) * grid%d1%x  &
               + ( fy(i,j+1,k)-fy(i,j-1,k) ) * grid%d1%y  &
               + ( fz(i,j,k+1)-fz(i,j,k-1) ) * grid%d1%z

          ! 以下がナビエ・ストークス方程式

          !  密度の時間発展
          the_equation%density(i,j,k) = -divf*dt

          !  質量フラックスの時間発展（3成分）
          the_equation%flux%x(i,j,k) =  &
               ( - divfvx  &
                 - gradpx  &
                 + Drive_force%x(i,j,k)*factor  &
                 + Viscosity * ( lapvx + ONE_THIRD*gdivvx )  &
               ) * dt
          the_equation%flux%y(i,j,k) =  &
               ( - divfvy  &
                 - gradpy  &
                 + Drive_force%y(i,j,k)*factor  &
                 + Viscosity * ( lapvy + ONE_THIRD*gdivvy )  &
               ) * dt
          the_equation%flux%z(i,j,k) =  &
               ( - divfvz  &
                 - gradpz  &
                 + Drive_force%z(i,j,k)*factor  &
                 + Viscosity * ( lapvz + ONE_THIRD*gdivvz )  &
               ) * dt

          !  圧力の時間発展
          the_equation%pressure(i,j,k) =  &
              ( - ( vx(i,j,k)*gradpx  &
                  + vy(i,j,k)*gradpy  &
                  + vz(i,j,k)*gradpz  &
                  )  &
                + Gamma1_kappa * laptm  &
                - GAMMA * ps(i,j,k) * divv(i,j,k)  &
              ) * dt
        end do
      end do
    end do

    call field__boundary_condition(the_equation)
      ! 境界条件の設定（周期境界条件）
      ! 上のdo loopは境界面上の格子点を除いた格子点（シミュレーション
      ! 領域の内部の格子点）上での値を計算するものであった。
      ! ここで境界面上の格子点の値の（更新されたばかりの）内部の
      ! 格子点上のデータをコピーすることで設定する。
    call debug__print("called solver/the_equation.")
      ! デバッグモードがonの時には毎回メッセージを標準出力に
      ! 書き出す。コードが完成し、product runの段階に入ったら
      ! デバッグモードをoffにすれば、書き出しは抑制される。
      ! この行をコメントアウトする必要はない。
  end function the_equation


!
! Private  これより上が非公開の関数・ルーチン
!===================================================
! Public   これより下が公開する関数・ルーチン
!


  subroutine solver__advance(t,dt,fluid)
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
    real(DR), intent(in)    :: dt !! 時間刻み幅
    type(field__fluid_t), intent(inout)  :: fluid !! 流体データ

    real(DR), parameter :: ONE_SIXTH = 1.0_DR / 6.0_DR ! 演算数節約の
    real(DR), parameter :: ONE_THIRD = 1.0_DR / 3.0_DR ! ために定義

    type(field__vector3d_t)       :: vel   ! 速度場
    real(DR), dimension(NX,NY,NZ) :: tm    ! 温度場
    real(DR), dimension(NX,NY,NZ) :: divv  ! 速度場の発散

    type(field__fluid_t) :: dfluid01, dfluid02, dfluid03, dfluid04
      ! 古典的な4段4次ルンゲ・クッタ積分法に必要な4つの作業配列
      ! サイズが大きいことに注意。
    type(field__fluid_t) :: gluid  ! work variable
      ! さらにもう一つの作業配列
      ! サイズが大きいことに注意。


    !---ルンゲ・クッタの第1段---!
    call subfield_vel_tm_divv(fluid,vel,tm,divv)
      ! 基本変数から副次的変数である速度、温度、速度の発散を求める
    dfluid01 = the_equation(t, dt,  &
                            vel%x, vel%y, vel%z, tm, divv,  &
                            fluid%flux%x, fluid%flux%y, fluid%flux%z,  &
                            fluid%pressure)

    t = t + dt/2
      ! 渦輪を駆動する力は時刻（t）の関数として設定しているので、
      ! いま解いているナビエ・ストークス方程式は時間に陽に依存する。

    !---ルンゲ・クッタの第2段---!
![  gluid = fluid + dfluid01*0.5_DR
    dfluid01 = operator_fluid_times_real(dfluid01,0.5_DR)
    gluid    = operator_fluid_add(fluid,dfluid01)
      ! Fortranコンパイラが自己定義演算子をきちんと処理できる
      ! 場合は上の簡潔な記述の方が（読みやすいので）好ましい。

    call subfield_vel_tm_divv(gluid,vel,tm,divv)
    dfluid02 = the_equation(t, dt,  &
                            vel%x, vel%y, vel%z, tm, divv,  &
                            gluid%flux%x, gluid%flux%y, gluid%flux%z,  &
                            gluid%pressure)

    !---ルンゲ・クッタの第3段---!
![  gluid = fluid + dfluid02*0.5_DR
    dfluid02 = operator_fluid_times_real(dfluid02,0.5_DR)
    gluid    = operator_fluid_add(fluid,dfluid02)
      ! Fortranコンパイラが自己定義演算子をきちんと処理できる
      ! 場合は上の簡潔な記述の方が（読みやすいので）好ましい。

    call subfield_vel_tm_divv(gluid,vel,tm,divv)
    dfluid03 = the_equation(t, dt,  &
                            vel%x, vel%y, vel%z, tm, divv,  &
                            gluid%flux%x, gluid%flux%y, gluid%flux%z,  &
                            gluid%pressure)

    t = t + dt/2
      ! 繰り返すが、渦輪を駆動する力は時刻（t）の関数として設定しているので、
      ! いま解いているナビエ・ストークス方程式は時間に陽に依存する。

    !---ルンゲ・クッタの第4段---!
![  gluid = fluid + dfluid03
    gluid = operator_fluid_add(fluid,dfluid03)
      ! Fortranコンパイラが自己定義演算子をきちんと処理できる
      ! 場合は上の簡潔な記述の方が（読みやすいので）好ましい。

    call subfield_vel_tm_divv(gluid,vel,tm,divv)
    dfluid04 = the_equation(t, dt,  &
                            vel%x, vel%y, vel%z, tm, divv,  &
                            gluid%flux%x, gluid%flux%y, gluid%flux%z,  &
                            gluid%pressure)

    !--- 最終結果 ---!

![  fluid = fluid  &
![        + ONE_SIXTH*( dfluid01 + 2*dfluid02 + 2*dfluid03 + dfluid04 )
    dfluid01 = operator_fluid_times_real(dfluid01,ONE_SIXTH)
    dfluid02 = operator_fluid_times_real(dfluid02,ONE_THIRD)
    dfluid03 = operator_fluid_times_real(dfluid03,ONE_THIRD)
    dfluid04 = operator_fluid_times_real(dfluid04,ONE_SIXTH)
    fluid = operator_fluid_add(fluid,dfluid01)
    fluid = operator_fluid_add(fluid,dfluid02)
    fluid = operator_fluid_add(fluid,dfluid03)
    fluid = operator_fluid_add(fluid,dfluid04)
      ! Fortranコンパイラが自己定義演算子をきちんと処理できる
      ! 場合は一番上の簡潔な記述の方が（読みやすいので）好ましい。

    call debug__print("called solver__advance.")
      ! デバッグモードがonの時には毎回メッセージを標準出力に
      ! 書き出す。コードが完成し、product runの段階に入ったら
      ! デバッグモードをoffにすれば、書き出しは抑制される。
      ! この行をコメントアウトする必要はない。
  end subroutine solver__advance


  subroutine solver__diagnosis(nloop,time,fluid)
    !! 流体の「健康状態」を診断する
    integer(DI),          intent(in) :: nloop  !! ループカウンタ
    real(DR),             intent(in) :: time   !! シミュレーション時刻
    type(field__fluid_t), intent(in) :: fluid  !! 流体データ

    integer(SI), parameter :: SKIP = 100
      ! このルーチンは結構計算負荷が高いので、
      ! 毎ステップではなく、SKIPステップごとに診断を実行する

    real(DR), parameter :: ABNORMALLY_LARGE = 1.e20_DR
      ! これよりも物理量が大きくなったら異常が生じたと判断する

    type(field__vector3d_t) :: vel
      ! 流れの速度場

    if ( mod(nloop,SKIP) /= 0 ) return
      ! このルーチンは結構計算負荷が高いので、
      ! 毎ステップではなく、SKIPステップごとに診断を実行する

    if ( job__karte%state /= "fine" ) return ! Already in error state.
      ! ジョブの健康状態がfine（つまり健康）以外の値に
      ! 設定する可能性があるのはここ以外にもある（例えばmain.f90の
      ! メインループでシミュレーションのループカウンタが最大値が達するなど）
      ! そのような場合、どうせこの後、ジョブの停止処理に入るので、
      ! これ以上計算を進めなくてもよい。

    if ( maxval(fluid%flux%x) > ABNORMALLY_LARGE ) then
      ! 質量フラックスのx成分の最大値が異常に大きい
      ! これはバグである。absを入れ忘れている。
      call ut__message("<solver__diagnosis> Massflux_x overflow.")
      call job__karte%set("over_flow")
      return
        ! これ以上計算しても無駄（すぐに終了処理に入るべし）
    end if

    if ( maxval(fluid%flux%y) > ABNORMALLY_LARGE ) then
      ! 質量フラックスのy成分の最大値が異常に大きい
      ! これもバグである。absを入れ忘れている。
      call ut__message("<solver__diagnosis> Massflux_y overflow.")
      call job__karte%set("over_flow")
      return
        ! これ以上計算しても無駄（すぐに終了処理に入るべし）
    end if

    if ( maxval(fluid%flux%z) > ABNORMALLY_LARGE ) then
      ! 質量フラックスのz成分の最大値が異常に大きい
      ! これもバグである。absを入れ忘れている。
      call ut__message("<solver__diagnosis> Massflux_z overflow.")
      call job__karte%set("over_flow")
      return
        ! これ以上計算しても無駄（すぐに終了処理に入るべし）
    end if

    if ( maxval(fluid%density) > ABNORMALLY_LARGE ) then
      ! 質量が異常に大きい
      call ut__message("<solver__diagnosis> Density overflow.")
      call job__karte%set("over_flow")
      return
        ! これ以上計算しても無駄（すぐに終了処理に入るべし）
    end if

    if ( maxval(fluid%pressure) > ABNORMALLY_LARGE ) then
      ! 圧力が異常に大きい
      call ut__message("<solver__diagnosis> Pressure overflow.")
      call job__karte%set("over_flow")
      return
        ! これ以上計算しても無駄（すぐに終了処理に入るべし）
    end if

    if ( minval(fluid%pressure) < 0.0_DR ) then
      ! 圧力が負になってしまっている
      call ut__message("<solver__diagnosis> Negative pressure.")
      call job__karte%set("negative_anormaly")
      return
        ! これ以上計算しても無駄（すぐに終了処理に入るべし）
    end if

    if ( minval(fluid%density) < 0.0_DR ) then
      ! 密度が負になってしまっている
      call ut__message("<solver__diagnosis> Negative density.")
      call job__karte%set("negative_anormaly")
      return
        ! これ以上計算しても無駄（すぐに終了処理に入るべし）
    end if

    call subfield_vel(fluid,vel)
      ! 基本流れデータから副次的な速度場データを求める

    call ut__message('#max vel:',      nloop, time,  &
                     sqrt(maxval(vel%x**2+vel%y**2+vel%z**2)))
      ! 速度の最大値（ベクトルの長さ）を計算し標準出力に書き出す
      ! わずか1行で書いているが、実際にはここにかなりの計算が
      ! 含まれている。vel%x**2という配列演算は実際には3重do loop
      ! であり、maxval関数はその引数の3次元配列をとっている。
      ! つまり全要素中の最大値をとっている。そして最後に
      ! sqrtをとって振幅（ベクトルの長さ）を計算している。

    call ut__message('#flow energy: ', nloop, time,  &
![                                    .energyintegral.fluid)
                                       operator_energyintegral(fluid))
      ! ここでもFortranコンパイラが許せば.energyintegral.という
      ! 簡潔な演算子表現を使ったほうがよい。ここでもこの1行の
      ! 実行にはかなりの演算（速度ベクトル場のx,y,z3成分の2乗和に
      ! 資料密度を掛けたもの体積積分）が必要であることに注意。

    call ut__message('#total mass: ',  nloop, time,  &
![                                    .scalarintegral.(fluid%density))
                               operator_scalarintegral(fluid%density))
      ! 上と同様。こちらのほうは単なる密度場の体積積分なので
      ! 演算量は少ないが、シミュレーション領域全体に渡る体積積分
      ! なので計算量は大きいことに間違いない。

    call debug__print('called solver__diagnosis.')
      ! デバッグモードがonの時には毎回メッセージを標準出力に
      ! 書き出す。コードが完成し、product runの段階に入ったら
      ! デバッグモードをoffにすれば、書き出しは抑制される。
      ! この行をコメントアウトする必要はない。
  end subroutine solver__diagnosis


  subroutine solver__initialize(fluid)
    !! モジュールの初期化
    type(field__fluid_t), intent(out) :: fluid  !! 流体データ

    real(DR) :: kappa  ! 空気の熱拡散率

    ! 物理パラメータの設定
    Viscosity = params__get_double('Viscosity') ! 空気の粘性率
    kappa     = params__get_double('Kappa')     ! 空気の熱拡散率
      ! Viscosityと違ってkappaの頭文字が大文字になっていない、つまり
      ! このモジュールのグローバルスコープを持つ変数としていない
      ! のはナビエ・ストークス方程式には下で定義するGamma1_kappa
      ! という量のみを通じてkappaが出てくるからである。

    Gamma1_kappa = (Gamma-1)*kappa
      ! gammaは比熱比、つまり定積比熱と定圧比熱の比である。
      ! 統計力学で習うように、この値は流体（気体）を構成する
      ! 分子の構造（自由度）で決まる。

    ! 流体の初期条件の設定
    fluid%pressure = 1.013e5_DR  ! 1013 hPa (一気圧)
    fluid%density  = 1.293_DR    ! kg/m^3 (空気の密度)
![  fluid%flux     = 0.0_DR      ! 初期速度なし（流れなし）
    fluid%flux%x   = 0.0_DR      ! 初期速度なし（流れなし）
    fluid%flux%y   = 0.0_DR        
    fluid%flux%z   = 0.0_DR       
      ! コンパイラが許す場合は上の簡潔な代入表現の方が望ましい

    ! 渦輪を駆動するための力の場の設定
    call set_drive_force_field

    Initialize_done = .true.
      ! モジュール初期化終了フラグ

    call debug__print("called solver__initialize.")
      ! デバッグモードがonの時には毎回メッセージを標準出力に
      ! 書き出す。コードが完成し、product runの段階に入ったら
      ! デバッグモードをoffにすれば、書き出しは抑制される。
      ! この行をコメントアウトする必要はない。
  end subroutine solver__initialize


  function solver__set_time_step(nloop,fluid)
    !! CFL条件に基づいて時間刻み幅dtを設定する
    integer(DI),          intent(in) :: nloop  !! ループカウンタ
    type(field__fluid_t), intent(in) :: fluid  !! 流体データ
    real(DR) :: solver__set_time_step !! 時間刻み幅 dt

    type(field__vector3d_t)       :: vel ! 速度場
    real(DR), dimension(NX,NY,NZ) :: tm  ! 温度場

    real(DR) :: vmax, sound_v 
      ! 流れ速度の最大値と音速
    real(DR) :: dt_vel, dt_sound, dt_viscous, dt_kappa
      ! 流れ速度、音波、粘性拡散、熱拡散、それぞれで決まる時間刻み幅
      ! 実際のdtはこれらのなかで最も厳しい（小さい）値できまる。
    real(DR), parameter :: ALMOST_ZERO = 1.e-20_DR 
      ! ゼロ割り演算回避のためにつかう小さい値
    real(DR), parameter :: ABNORMAL_VALUE = -999.999_DR 
      ! dtとしてありそうにない値（すぐ下で使う）
    real(DR), save :: dt = ABNORMAL_VALUE
      ! 時間刻み幅そのもの。初期設定でなんらかの失敗した場合
      ! を検出するため最初はありそうない値を設定しておくが、
      ! CFL条件に基づいて正しい値を一度設定したらその後は
      ! この宣言文にはsave属性がついているので、
      ! この関数を抜けたあともその値を覚えている。
    integer(SI), parameter :: SKIP = 20
      ! dtを毎ステップ計算するのは大変（かなりの時間がかかる）
      ! のでこのSKIPステップに一度だけ計算する。毎ステップ計算する
      ! してもdtは1ステップでそれほど大きく変化しないからである。
      ! ただし、計算が破綻（発散）するような場合は例外である。

    call ut__assert(Initialize_done,"<solver__set_tim_step> Forgot init?")
      ! 初期化忘れ確認

    if ( mod(nloop,SKIP)==0 ) then ! ほとんどの場合は、前回計算したdtを使う。
      call subfield_vel_tm(fluid,vel,tm)
        ! 基本流体データから速度場と温度場を計算

      vmax = maxval(sqrt(vel%x**2+vel%y**2+vel%z**2))
        ! 速度の最大値（ベクトルの長さの最大値）をもとめる
        ! ここでは配列演算を駆使して1行で書いているが
        ! 実際にはかなりの演算をしていることに注意。
        ! これはバグではないが、maxvalとsqrtは交換すべき
        ! だろう。このままだと3次元配列の全要素にsqrt
        ! を掛けてからそのmaxvalをとっているが、これを
        ! 逆にした方が速いかもしれない。

      vmax = max(vmax,ALMOST_ZERO) 
        ! 初期条件では速度場がゼロなのでvmax=0となるが、あとで
        ! vmaxの割り算が出てくるので問題となる。それを回避するため
        ! ALMOST_ZEROが十分小さければよい。

      sound_v = GAMMA*maxval(sqrt(GASS_CONST_FOR_AIR*tm)) 
        ! 音速の最大値
        ! これも上と同じ理屈で、maxvalとsqrtは交換すべき
        ! だろう。このままだと3次元配列の全要素にsqrt
        ! を掛けてからそのmaxvalをとっているが、これを
        ! 逆にした方が速いかもしれない。

      call ut__assert(sound_v > ALMOST_ZERO,"<solver__time_step> sound_v=0?")
        ! 音速（の最大値）がほとんどゼロになるのは
        ! 何かがおかしいのですぐに停止

      ! 以下では、流れの速さ、音波、粘性拡散、熱拡散の4種類の
      ! CFL条件で決まる時間刻み幅をそれぞれこの順番に求めている。
      dt_vel     = 0.8_DR*grid%delta_min/vmax
      dt_sound   = 0.8_DR*grid%delta_min/sound_v
      dt_viscous = 0.2_DR*(grid%delta_min**2)/Viscosity
      dt_kappa   = 0.2_DR*(grid%delta_min**2)/Gamma1_kappa
        ! CFL factor はここでは0.8と0.2にしているが、
        ! これは精密な議論に基づいて決めたものではなく、
        ! 半経験的に決めた値である。
        ! 最後の2つ、粘性拡散と熱拡散によるCFL条件のための
        ! CFL factor の値 (0.2) というのは少々安全側に設定
        ! しすぎているかもしれない。つまりもう少し大きくしても
        ! 問題ないかもしれない。

      dt = min(dt_vel, dt_sound, dt_viscous, dt_kappa)
        ! 最終的な時間刻み幅は上記の4種類のdtの最小値できまる。

      if ( params__get_logical('Debug') ) then
        ! デバッグモードがonであるとき、以下のデータを標準出力
        ! に書き出す。出力が長くなるが、流体の状態を推測するのに
        ! 便利なデータである。
        call ut__message('<solver__time_step> vmax = ', vmax       )
        call ut__message('                  dt_vel = ', dt_vel     )
        call ut__message('                dt_sound = ', dt_sound   )
        call ut__message('                dt_kappa = ', dt_kappa   )
        call ut__message('              dt_viscous = ', dt_viscous )
        call ut__message('               -->    dt = ', dt         )
      end if

      if ( mod(nloop,SKIP*50)==0 )  call ut__message("> dt = ", dt)
        ! dtの値を（デバッグモードがoffであっても）書き出す
        ! SKIPをさら50倍しているのは、それほど頻繁に出力する
        ! 必要は通常ないからである。

    end if

    call ut__assert(dt /= ABNORMAL_VALUE, "<solver__time_step> dt init failed?")
      ! SKIPに一度dtを計算し直すが、それ以外は下の行を見れば分かる通り
      ! 前回のdtの値を流用する。 その際、一度もdtを計算したことがなかったら
      ! まずいのでその検出をABNORMAL_VALUEを使って検出している。

    solver__set_time_step = dt    ! dt of the prev calc is saved.
      ! 新たに更新（または前回計算した）dtを返す。
  end function solver__set_time_step

end module solver_m
