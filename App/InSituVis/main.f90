!*******************************************************************
!> author: Akira Kageyama
!  date: 2020.01.22
!
!  渦輪 (smoke ring) の形成シミュレーション
!
!  神戸大学情報知能工学科の講義 "HPC" （B3対象）用サンプルコード
! 
!  ### 形状
!    直方体領域。3次元周期境界条件。カーテシアン座標。
! 
!  ### 計算手法
!    空間離散化は2次中心差分法。時間積分は4次ルンゲ・クッタ法。
! 
!  ### 実行方法
!    (1) cd src
!    (2) make
!    (3) cd ../slice_grapher
!    (4) make
!     
program main_m
!  use constants_m  ! 定数
!  use ut_m         ! ユーティリティ
!  use params_m     ! パラメータ
!  use debug_m      ! デバッグ
!  use grid_m       ! 格子点情報
!  use field_m      ! 流体場の構造体定義
!  use slicedata_m  ! 可視化用断面データ出力
!  use solver_m     ! ナビエ・ストークス方程式ソルバ
!  use job_m        ! ジョブ管理
!  use vis_m
  use smoke_ring_m
  use InSituVis_m

  use, intrinsic :: iso_fortran_env
  implicit none    ! 暗黙の型宣言無効化。必須

  integer(DI) :: nloop ! シミュレーションのループカウンタ
  real(DR) :: dt, time ! 時間刻み幅と時刻

  type(field__fluid_t) :: fluid ! 流体場データの構造体

  integer :: visualization_count =0 !何回可視化されたか
  integer :: job_count = 0 !何回目のジョブか（接続ジョブ用
  character(100) :: arg_job

  integer ::unit

  ! IN_SITU_VIS: Instance
  ! {
  type( InSituVis ) :: insitu_vis
  insitu_vis = InSituVis( VolumeRendering ) ! OrthoSlice, Isosurface, or VolumeRendering
  ! }

  call params__read
    ! パラメーターの読み込み。paramsの後に続くアンダースコア
    ! 二つは、これがparamsモジュールの中にあるサブルーチンの
    ! 呼び出しであることを意味している。paramsモジュールの
    ! 名前はparams_mである。params_mはparams.f90にある。
  time = 0.0_DR  ! 時刻の初期化。単位は秒。
  nloop = 0      ! ループカウンタの初期化。

  call get_command_argument(2,arg_job)
  read(arg_job,*) job_count
  ! nloop = (job_count-1) * params__get_integer('Total_nloop')
  
  call grid%initialize
    ! gridモジュールの初期化。
    ! パーセント記号はメンバアクセス演算子。
    ! ここでは構造体のメンバー関数の呼び出しをしている。
    ! gridモジュール（grid_m）はgrid.f90で定義されている。
  ! if(job_count /=1) then

  ! open(newunit=unit, file="vis_count.dat",status='replace')
  ! read(unit,*) visualization_count
  ! close(unit)

  ! open(newunit=unit,file="density.dat",form='unformatted',status='replace')
  ! read(unit) fluid%density
  ! close(unit)
  ! open(newunit=unit,file="pressure.dat",form='unformatted',status='replace')
  ! read(unit) fluid%pressure
  ! close(unit)
  ! open(newunit=unit,file="flux_x.dat",form='unformatted',status='replace')
  ! read(unit) fluid%flux%x
  ! close(unit)
  ! open(newunit=unit,file="flux_y.dat",form='unformatted',status='replace')
  ! read(unit) fluid%flux%y
  ! close(unit)
  ! open(newunit=unit,file="flux_z.dat",form='unformatted',status='replace')
  ! read(unit) fluid%flux%z
  ! close(unit)

  ! end if

  call solver__initialize(fluid)
    ! solverジュール（solver_m）の初期化。ここでは上の
    ! gridモジュールの場合と異なりメンバアクセス演算子
    ! （パーセント記号）を使っていない理由は特にない。
  call slicedata__initialize
    ! slicedataモジュールの初期化。
    ! このモジュールはシミュレーション領域の断面図を出力する。

  call solver__diagnosis(nloop,time,fluid)
    ! solverモジュールで定義されているdiagnosis（診断）
    ! サブルーチンを呼び出す。医者が患者を診るのがdiagnosis
    ! である。そこでの診断結果はjobモジュールのjob__carte
    ! という構造体にセットする。carteはカルテである。

  dt = solver__set_time_step(nloop,fluid)
    ! 時間刻み幅 dt の決定。dtはCFL条件を満足するように決めるが、
    ! CFL条件は流体の状態に流体の状態に依存して変化する。
    ! たとえば、流体の一部が高温になると、そこでの音速が速くなり、
    ! 音速によって決まるCFL条件が厳しくなる（つまりdtが小さくなる）
    ! ここでは初期状態における流体の状態に基づいてdtが決まる

  ! IN_SITU_VIS: Initialize
  ! {
  call insitu_vis % initialize()
  ! }

  !  call vis%initialize(job_count)

  do while(job__karte%state=="fine")
    ! このシミュレーションのメインループである。ジョブカルテが
    ! 「健康 (fine)」状態である限りシミュレーションを続行する。 
    call debug__print("running. nloop=",nloop)
      ! このdebugモジュール中の標準出力書き出しルーチンの
      ! 呼び出し。通常のプリント文と異なりデバッグフラグがtrue 
      ! の時だけメッセージを書き出すような仕組みにしている。
      ! デバッグフラグがfalseのときには何も書き出さない。
      ! デバッグフラグはparamsモジュール内で定義している。
    call solver__advance(time,dt,fluid)
      ! ナビエ・ストークス方程式に基づいて流体 (fluid) の状態を
      ! 一時刻ステップ dt だけ進める。
    dt = solver__set_time_step(nloop,fluid)
      ! 流体の状態が変わったのでCFL条件に基づき時間刻み幅dt
      ! を設定し直す。
      ! 厳密に言えば毎ステップこの再設定をしているわけではなく、
      ! このsolver__set_time_stepルーチンの冒頭で判断し、
      ! 数十ステップに一度だけ実際には変更を行うようなskip操作
      ! をしている。CFL条件に基づいた計算は時間がかかるが、
      ! 毎ステップdtを精密に調整する必要はないからである。

    nloop = nloop + 1  ! ループカウンタのインクリメント
    call solver__diagnosis(nloop,time,fluid)
      ! 診断。異常があればjob__carteにセットする。
    ! call slicedata__write(nloop,time,fluid)
      ! 断面データのディスクへの書き出し

    ! IN_SITU_VIS: Put & Execute
    ! {
    !call insitu_vis % put( fluid % pressure, NX, NY, NZ )
    call insitu_vis % put( vis % get_enstrophy( fluid ), NX, NY, NZ )
    call insitu_vis % exec( time, nloop )
    ! }

    !可視化部分
!    call vis%visualize(nloop,fluid,visualization_count)
    visualization_count = visualization_count +1

    if (nloop>=params__get_integer('Total_nloop'))  &
      call job__karte%set("loop_max")
      ! あらかじめparamsモジュールで設定されたループカウンタの
      ! 上限値に達したらジョブを停止する。

  end do

  ! IN_SITU_VIS: Finalize
  ! {
  call insitu_vis % finalize()
  call insitu_vis % delete()
  ! }

  !  call vis%finalize
  call job__finalize(nloop)
      ! ジョブの後始末。実際にはメーセージを標準出力に書くだけ。
end program main_m
