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
  use kvs_m

  use, intrinsic :: iso_fortran_env
  implicit none    ! 暗黙の型宣言無効化。必須

  integer(DI) :: nloop ! シミュレーションのループカウンタ
  real(DR) :: dt, time ! 時間刻み幅と時刻

  type(field__fluid_t) :: fluid ! 流体場データの構造体

  integer :: visualization_count =0 !何回可視化されたか
  integer :: job_count = 0 !何回目のジョブか（接続ジョブ用
  character(100) :: arg_job

  integer ::unit

  ! IN_SITU_VIS: Visualization setup
  ! {
  type( kvs_OffScreen )              :: screen   !! rendering screen
  type( kvs_StructuredVolumeObject ) :: volume   !! structured volume object
  type( kvs_Bounds )                 :: bounds   !! bounding box module
  type( kvs_ColorMap )               :: cmap     !! color map
  type( kvs_OpacityMap )             :: omap     !! opacity map
  type( kvs_TransferFunction )       :: tfunc    !! transfer function
  type( kvs_RayCastingRenderer )     :: renderer !! ray casting renderer
  type( kvs_ColorImage )             :: image    !! rendering image
  character( len = 100 )             :: filename !! output filename
  ! }

  ! IN_SITU_VIS: Screen settings
  ! {
  screen = kvs_OffScreen()
  call screen % setCameraPosition( kvs_Vec3( 7, 5, 6 ) )
  call screen % setLightPosition( kvs_Vec3( 7, 5, 6 ) )
  call screen % create()
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

    ! IN_SITU_VIS: Create volume
    ! {
    volume = kvs_StructuredVolumeObject()
    call volume % setName( "Volume" )
    call volume % setGridTypeToUniform()
    call volume % setVeclen( 1 )
    call volume % setResolution( kvs_Vec3i( NX, NY, NZ ) )
    !call volume % setValues( fluid % pressure, NX * NY * NZ )
    call volume % setValues( vis % get_enstrophy( fluid ), NX * NY * NZ )
    call volume % updateMinMaxValues()
    call volume % updateMinMaxCoords()
    ! }

    ! IN_SITU_VIS: Visualization
    ! {
    if ( screen % hasObject( "Volume" ) ) then
       call screen % replaceObject( "Volume", volume % get() )
    else
       bounds = kvs_Bounds()
       call screen % registerObject( volume % get(), bounds % get() )

       cmap = kvs_ColorMap_CoolWarm()
       omap = kvs_OpacityMap()
       call omap % addPoint(   0.0, 0.0 )
       call omap % addPoint(   1.0, 0.2 )
       call omap % addPoint( 250.0, 0.5 )
       call omap % addPoint( 253.0, 0.1 )
       call omap % addPoint( 255.0, 0.2 )
       call omap % create()

       tfunc = kvs_TransferFunction()
       call tfunc % setColorMap( cmap )
       call tfunc % setOpacityMap( omap )
       call tfunc % setRange( volume % minValue(), volume % maxValue() )

       renderer = kvs_RayCastingRenderer( glsl = .false. )
       call renderer % setTransferFunction( tfunc )
       call screen % registerObject( volume % get(), renderer % get() )
    end if
    call screen % draw()
    ! }

    ! IN_SITU_VIS: Output rendering image
    ! {
    write ( filename, '("output_", i6.6, ".bmp")' ) nloop
    image = screen % capture()
    call image % write( trim( filename ) )
    call image % delete()
    ! }

    !可視化部分
!    call vis%visualize(nloop,fluid,visualization_count)
    visualization_count = visualization_count +1

    if (nloop>=params__get_integer('Total_nloop'))  &
      call job__karte%set("loop_max")
      ! あらかじめparamsモジュールで設定されたループカウンタの
      ! 上限値に達したらジョブを停止する。

  end do

  !  call vis%finalize
  call job__finalize(nloop)
      ! ジョブの後始末。実際にはメーセージを標準出力に書くだけ。

end program main_m
