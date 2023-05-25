!!>
!   author: Akira Kageyama
!   date: 2023.05.05
! 
!   格子点関連情報
! 
!   @note 
!      モジュール名と同じgridというpublicな変数（構造体）を
!      定義し、その実体をこのモジュール内に保持している。
!!<
module grid_m
  use constants_m  ! 定数定義
  use parallel_m   ! 並列化モジュール
  use ut_m         ! ユーティリティ
  implicit none    ! 暗黙の型宣言無効化。必須
  private          ! このモジュール内の変数等はデフォルトでは非公開

  type grid__pos_t
    !! 格子点位置を収める構造体
    !! posはpositionの意味
    real(DR), dimension(0:NXPP1) :: x !! x座標（MPI領域分割した
    real(DR), dimension(0:NYPP1) :: y !! y座標  各プロセスでの
    real(DR), dimension(0:NZPP1) :: z !! z座標  局所的な番号）
  end type grid__pos_t

  type grid_pos_by_global_ijk_t
    real(DR), dimension(NX_GLOBAL) :: x  !! シミュ領域全体にわたる
    real(DR), dimension(NY_GLOBAL) :: y  !! 格子点番号 global な
    real(DR), dimension(NZ_GLOBAL) :: z  !! 番号という意味でgi等とよぶ
  end type grid_pos_by_global_ijk_t

  type grid_global_ijk_t  !! global (gi,gj,gk) from local (li,lj,lk)
    integer, dimension(0:NXPP1) :: gi !! MPIで領域分割した各プロセス
    integer, dimension(0:NYPP1) :: gj !! の局所的な格子点位置から
    integer, dimension(0:NZPP1) :: gk !! シミュ領域全体の格子点位置をとる
  end type grid_global_ijk_t

  type grid_local_ijk_from_global_ijk_t  !! The opposite of above.
    integer, dimension(NX_GLOBAL) :: li  !! グローバルな格子点位置
    integer, dimension(NY_GLOBAL) :: lj  !! から局所的な格子点位置を
    integer, dimension(NZ_GLOBAL) :: lk  !! 求める
  end type grid_local_ijk_from_global_ijk_t

  type grid__delta_t
    !! 格子間隔構造体
    real(DR) :: x  !! x方向の格子間隔
    real(DR) :: y  !! y方向の格子間隔
    real(DR) :: z  !! z方向の格子間隔
  end type grid__delta_t

  type grid__derivative_operator_1st_t
    !! 1階微分（差分）をとる時の演算子（定数）
    !! 演算回数の節約のため
    real(DR) :: x  !! x偏微分
    real(DR) :: y  !! y偏微分
    real(DR) :: z  !! z偏微分
  end type grid__derivative_operator_1st_t

  type grid__derivative_operator_2nd_t
    !! 2階微分（差分）をとる時の演算子（定数）
    !! 演算回数の節約のため
    real(DR) :: x   !! x偏微分
    real(DR) :: y   !! y偏微分
    real(DR) :: z   !! z偏微分
  end type grid__derivative_operator_2nd_t

  type :: grid_t
    !! 格子点関係のデータを全て収める構造体
    !! このモジュールの中心変数。これを介して外部と
    !! やり取りする。このグリッド変数の定義 (下記のgrid_t）
    !! と実体（このgrid）が同じ場所（このgrid_mモジュール内）
    !! にあるのは少々変則的かもしれないが、このように規模の
    !! 小さいシミュレーションプログラムではこの方が
    !! むしろわかりやすいかもしれない。
    type(grid__pos_t) :: pos                     !! 格子点位置
    type(grid_pos_by_global_ijk_t) :: pos_by_global_ijk
    type(grid_global_ijk_t) :: global_ijk
    type(grid_local_ijk_from_global_ijk_t) :: local_ijk_from_global_ijk
    type(grid__delta_t) :: delta                 !! 格子間隔
    real(DR) :: delta_min                        !! 最小の格子間隔
    type(grid__derivative_operator_1st_t) :: d1  !! 1階微分演算子定数
    type(grid__derivative_operator_2nd_t) :: d2  !! 2階微分演算子定数
  contains
    procedure :: initialize => grid__initialize  
      !! 初期化関数
      !! 初期化のためのメンバー関数
      !! こうするとgrid%initializeという形でcallできる
    procedure :: i_have_gi => grid__i_have_gi
      !! global格子点giの位置をそのプロセスが含むかどうか。
    procedure :: i_have_gj => grid__i_have_gj
      !! global格子点gjの位置を
    procedure :: i_have_gk => grid__i_have_gk
      !! global格子点gkの位置を
    procedure :: i_have_gijk => grid__i_have_gijk
      !! global格子点(gi,gj,gk)をそのプロセスが持っている
      !! かどうか。
  end type grid_t

  type(grid_t), public :: Grid  ! 実体


contains


  function initialize_global_ijk() result(gijk)
    type(grid_global_ijk_t) :: gijk

    integer :: i, j, k

    do i = 0, NXPP1
      gijk%gi(i) = NXPP*Parallel%pos%index%i + i + 1
    end do

    do j = 0, NYPP1
      gijk%gj(j) = NYPP*Parallel%pos%index%j + j + 1
    end do

    do k = 0, NZPP1
      gijk%gk(k) = NZPP*Parallel%pos%index%k + k + 1
    end do
  end function initialize_global_ijk


  function initialize_local_ijk_from_global_ijk( gijk ) result(ans)
    type(grid_global_ijk_t), intent(in) :: gijk
    type(grid_local_ijk_from_global_ijk_t) :: ans
    !!>
!      When NXPP = 4 and NPROC_X = 2, NX_GLOBAL = 10
!      
!              gi=> 1   2   3   4   5   6   7   8   9   10
!                   o---o---o---o---o---o---o---o---o---o
!            gi-1=> 0   1   2   3   4   5   6   7   8   9
!                   |                   |               .
!                   |      [rank 0]     |               .
!                   |                   |               .
!             li => 0   1   2   3   4   5               .
!                   |   |   |   |   |   |               .
!                   0   1   2   3   4   5 <= gi-1-0*NXPP.
!                   o---o---o---o---o---o---o---o---o---o
!                                   |                   |
!                                   |      [rank 1]     |
!                                   |                   |
!                            li =>  0   1   2   3   4   5
!                                   |   |   |   |   |   |
!                     gi-1-1*NXPP=> 0   1   2   3   4   5
    !!<
    integer :: gi, gj, gk

    do gi = 1, NX_GLOBAL
      if ( gi < gijk%gi(0) .or. gi > gijk%gi(NXPP1) ) then
        ans%li(gi) = NIL   ! out of scope of this process
      else
        ans%li(gi) = gi - 1 - NXPP*Parallel%pos%index%i
      end if
    end do

    do gj = 1, NY_GLOBAL
      if ( gj < gijk%gj(0) .or. gj > gijk%gj(NYPP1) ) then
        ans%lj(gj) = NIL   ! out of scope of this process
      else
        ans%lj(gj) = gj - 1 - NYPP*Parallel%pos%index%j
      end if
    end do  

    do gk = 1, NZ_GLOBAL
      if ( gk < gijk%gk(0) .or. gk > gijk%gk(NZPP1) ) then
        ans%lk(gk) = NIL   ! out of scope of this process
      else
        ans%lk(gk) = gk - 1 - NZPP*Parallel%pos%index%k
      end if
    end do      

  end function initialize_local_ijk_from_global_ijk


  function initialize_pos_by_global_ijk( dx, dy, dz ) result(pos_gijk)
    real(DR), intent(in) :: dx, dy, dz
    type(grid_pos_by_global_ijk_t) :: pos_gijk

    integer :: gi, gj, gk

    do gi = 1, NX_GLOBAL
      pos_gijk%x(gi) = XMIN + dx * ( gi - 1 )
    end do

    do gj = 1, NY_GLOBAL
      pos_gijk%y(gj) = YMIN + dy * ( gj - 1 )
    end do

    do gk = 1, NZ_GLOBAL
      pos_gijk%z(gk) = ZMIN + dz * ( gk - 1 )
    end do

  end function initialize_pos_by_global_ijk


  function initialize_pos( gijk, pos_by_gijk ) result(pos)
    type(grid_global_ijk_t), intent(in) :: gijk
    type(grid_pos_by_global_ijk_t), intent(in) :: pos_by_gijk
    type(grid__pos_t) :: pos

    integer :: i, j, k

    do i = 0, NXPP1
      pos%x(i) = pos_by_gijk%x( gijk%gi(i) )
    end do

    do j = 0, NYPP1
      pos%y(j) = pos_by_gijk%y( gijk%gj(j) )
    end do

    do k = 0, NZPP1
      pos%z(k) = pos_by_gijk%z( gijk%gk(k) )
    end do      

  end function initialize_pos


!
! Private  これより上が非公開の関数・ルーチン
!===================================================
! Public   これより下が公開する関数・ルーチン
!


  function grid__i_have_gi( grid, gi ) result(ans)
    class(grid_t), intent(in) :: grid
    integer, intent(in) :: gi
    logical :: ans

    ans = gi >= grid%global_ijk%gi(1)  .and.  &
          gi <= grid%global_ijk%gi(NXPP)
  end function grid__i_have_gi


  function grid__i_have_gj( grid, gj ) result(ans)
    class(grid_t), intent(in) :: grid
    integer, intent(in) :: gj
    logical :: ans

    ans = gj >= grid%global_ijk%gj(1)  .and.  &
          gj <= grid%global_ijk%gj(NYPP)
  end function grid__i_have_gj


  function grid__i_have_gk( grid, gk ) result(ans)
    class(grid_t), intent(in) :: grid
    integer, intent(in) :: gk
    logical :: ans

    ans = gk >= grid%global_ijk%gk(1)  .and.  &
          gk <= grid%global_ijk%gk(NZPP)
  end function grid__i_have_gk


  function grid__i_have_gijk( grid, gi, gj, gk ) result(ans)
    class(grid_t), intent(in) :: grid
    integer, intent(in) :: gi, gj, gk
    logical :: ans

    ans = grid%i_have_gi( gi ) .and.  &
          grid%i_have_gj( gj ) .and.  &
          grid%i_have_gk( gk )
  end function grid__i_have_gijk


  subroutine grid__initialize(grid)
    !! gridの初期化
    !! 
    !! ここでは周期境界条件を仮定している。
    !!
    !! 構造体のメンバー関数としてcallするときその
    !! 構造体変数そのものがgridとして自動的に引数にはいる。
    !! たとえば、
    !!     call sampl_grid%initialize
    !! は
    !!     call grid__initialize(sample_grid)
    !! と解釈される。gridという名前でなくても構わない。
    class(grid_t), intent(out) :: grid  !! 格子構造体

    real(DR) :: dx, dy, dz  ! 格子間隔

    !!>
!       周期境界条件
!      
!          --+-----+-----|                             |-----+-----+---
!            6     7     8                             1     2     3
!                  |-----+-----+-----+-----+-----+-----+-----|
!                  1     2     3     4     5     6     7     8
!                     |===================================|
!                    XMIN                                XMAX
    !!<
    dx = (XMAX-XMIN)/(NX_GLOBAL-2)  ! from (1.5) to (NX-0.5), see above figure.
    dy = (YMAX-YMIN)/(NY_GLOBAL-2)  ! ここではxminがi=1とi=2の2つの格子点の
    dz = (ZMAX-ZMIN)/(NZ_GLOBAL-2)  ! ちょうど中間に位置すると仮定している。

    grid%delta%x = dx ! x方向の格子間隔
    grid%delta%y = dy ! y方向の格子間隔
    grid%delta%z = dz ! z方向の格子間隔

    grid%delta_min = min(grid%delta%x,grid%delta%y,grid%delta%z)
      ! 3つの格子間隔の最小値。
      ! CFL条件はこの最小値で決まる。

    grid%d1%x = 1.0_DR/(2*dx)   ! 1階差分演算用定数
    grid%d1%y = 1.0_DR/(2*dy)   ! dはderivativeを表す
    grid%d1%z = 1.0_DR/(2*dz)   ! d1は1階差分の意味

    grid%d2%x = 1.0_DR/(dx**2)  ! 2階差分演算用定数
    grid%d2%y = 1.0_DR/(dy**2)  ! dはderivativeを表す
    grid%d2%z = 1.0_DR/(dz**2)  ! d2は2階差分の意味

    grid%global_ijk = initialize_global_ijk()
    grid%local_ijk_from_global_ijk  &
                    = initialize_local_ijk_from_global_ijk( grid%global_ijk )
    grid%pos_by_global_ijk = initialize_pos_by_global_ijk( dx, dy, dz )
    grid%pos = initialize_pos( grid%global_ijk, grid%pos_by_global_ijk )

  end subroutine grid__initialize

end module grid_m

