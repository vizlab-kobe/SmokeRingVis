!!>
!   author: Akira Kageyama
!   date: 2023.05.05
! 
!   渦輪 (smoke ring) の形成シミュレーション
! 
!   神戸大学情報知能工学科の講義 "HPC" （B3対象）用サンプルコード
!  
!   ### 形状
!     直方体領域。3次元周期境界条件。カーテシアン座標。
!  
!   ### 計算手法
!     空間離散化は2次中心差分法。時間積分は4次ルンゲ・クッタ法。
!  
!   ### 実行方法
!     (1) cd src
!     (2) make
!     (3) cd ../slice_grapher
!     (4) make
!!<
program main_m
!  use constants_m  !! 定数
!  use field_m      !! スカラー場・ベクトル場
!  use fluid_m      !! 流体場の構造体定義
!  use grid_m       !! 格子点情報
!  use job_m        !! ジョブ管理
!  use kutimer_m    !! 時間測定モジュール ;call kutimer__('main  ','sample')
!  use mpiut_m      !! mpi関係ユーティリティ
!  use parallel_m   !! MPI並列化
!  use params_m     !! パラメータ
!  use solver_m     !! ナビエ・ストークス方程式ソルバ
!  use ut_m         !! ユーティリティ
!  use vis2d_m      !! 断面可視化
  use smoke_ring_m
  use InSituVis_m
  implicit none    !! 暗黙の型宣言無効化。必須

  real(DR) :: dt, time   !! 時間刻み幅と時刻
  type(fluid_t) :: fluid !! 流体場データの構造体
!  type(vis2d_t) :: vis2d !! 断面可視化用

  ! IN_SITU_VIS: Parameters
  ! {
  type( InSituVis ) :: insitu_vis       !! in-situ vis. adaptor
  integer           :: dimx, dimy, dimz !! resolution of local grid
  integer           :: offx, offy, offz !! offset
  ! }

                                                   call kutimer__start('main  ')

  call Parallel%initialize                        ;call kutimer__('main  ','para i')
    !! MPI並列化初期化処理。Parallel変数はparallel.efで定義
    !! された parallel_t 型の変数。initializeはそのメンバー関数。
  call Job%initialize( time, fluid )              ;call kutimer__('main  ','job  i')
    !! シミュレーションジョブの初期化作業。初期条件の設定。
    !! 接続ジョブの場合はディスクからリスタートデータを読み込む。
  call iPrint                                     ;call kutimer__('main  ','iprint')
    !! 内部副プログラム。定数とパラメータを書き出す
!  call vis2d%initialize                           ;call kutimer__('main  ','vis2 i')
    !! 可視化モジュール（vis2d_m）の初期化。

  if ( Job%nloop == 0 ) then
!    call vis2d%draw( time, Job%nloop, fluid )     ;call kutimer__('main  ','vis2  ')
      !! シミュレーション領域の断面図をSVGフォーマットで出力する。
    call Job%diagnosis( Job%nloop, time, fluid )  ;call kutimer__('main  ','job di')
      !! solverモジュールで定義されているdiagnosis（診断）
      !! サブルーチンを呼び出す。診断結果はJob.carte（カルテ）に設定。
  end if

  dt = Solver%set_time_step( Job%nloop, fluid )   ;call kutimer__('main  ','set dt')
    !! 時間刻み幅 dt をCFL条件から決める。
    !! CFL条件は流体の状態に流体の状態に依存して変化する。
    !! たとえば、流体の一部が高温になると、そこでの音速が速くなり、
    !! 音速によって決まるCFL条件が厳しくなる（つまりdtが小さくなる）
    !! ここでは初期状態における流体の状態に基づいてdtが決まる。

  ! IN_SITU_VIS: Instance & Initialize
  ! {
  dimx = NXPP + 2
  dimy = NYPP + 2
  dimz = NZPP + 2
  offx = NXPP * ( mod( parallel % rank % me, NPROC_X ) )
  offy = NYPP * ( mod( parallel % rank % me, NPROC_X * NPROC_Y ) / NPROC_X )
  offz = NZPP * ( parallel % rank % me / ( NPROC_X * NPROC_Y ) )

  !insitu_vis = InSituVis( OrthoSlice ) ! OrthoSlice, Isosurface, or VolumeRendering
  insitu_vis = InSituVis( Isosurface ) ! OrthoSlice, Isosurface, or VolumeRendering
  !insitu_vis = InSituVis( VolumeRendering ) ! OrthoSlice, Isosurface, or VolumeRendering
  call insitu_vis % initialize()
  call insitu_vis % setGlobalDims( NX_GLOBAL, NY_GLOBAL, NZ_GLOBAL )
  call insitu_vis % setOffset( offx, offy, offz )
  ! }

  do while( Job%karte == "fine" )                 ;call kutimer__count
    !! このシミュレーションのメインループ。ジョブカルテが
    !! 「健康 (fine)」状態である限りシミュレーションを続行する。 
    Job%nloop = Job%nloop + 1  
      !! ループカウンタのインクリメント
    call Solver%advance( time, dt, fluid )        ;call kutimer__('main  ','solv a')
      !! ナビエ・ストークス方程式に基づいて流体 (fluid) の状態を
      !! 一時刻ステップ dt だけ進める。
    dt = Solver%set_time_step( Job%nloop, fluid ) ;call kutimer__('main  ','set dt')
      !! 流体の状態が変わったのでCFL条件に基づき時間刻み幅dt
      !! を設定し直す。
      !! 厳密に言えば毎ステップこの再設定をしているわけではなく、
      !! このsolver__set_time_stepルーチンの冒頭で判断し、
      !! 数十ステップに一度だけ実際には変更を行うようなskip操作
      !! をしている。CFL条件に基づいた計算は時間がかかるが、
      !! 毎ステップdtを精密に調整する必要はないからである。
    call Job%diagnosis( Job%nloop, time, fluid )  ;call kutimer__('main  ','job di')
      !! 診断。異常があればjob.carteにセットする。
!    call vis2d%draw( time, Job%nloop, fluid )     ;call kutimer__('main  ','vis2  ')
      !! シミュレーション領域の断面図をSVGで出力する。

    ! IN_SITU_VIS: Put & Execute
    ! {
    !call insitu_vis % put( fluid % pressure, dimx, dimy, dimz )
    call insitu_vis % put( get_enstrophy( fluid ), dimx, dimy, dimz )
    call insitu_vis % exec( time, job % nloop )
    ! }

    if ( Job%nloop >= Job%nloop_end ) then
      Job%karte = "loop_max"
      !! あらかじめparamsモジュールで設定されたループカウンタの
      !! 上限値に達したらジョブを停止する。
    end if
  end do

  ! IN_SITU_VIS: Finalize
  ! {
  call insitu_vis % finalize()
  call insitu_vis % delete()
  ! }

  call Job%finalize( Job%nloop, time, fluid )     !{main  }{job f}
    !! ジョブの後始末。MPI終了処理を含む。
                                                   call kutimer__end('main  ')
                                                   call kutimer__print
  call Parallel%finalize
    !! MPI並列化終了処理

contains

  subroutine iPrint
    integer :: comm
    comm = Parallel%comm
    call mpiut__message_leader( comm, "Job_name",     &
                                      Params%get_string( 'Job_name') )
    call mpiut__message_leader( comm, "Job_seq",     &
                                      Params%get_integer( 'Job_seq') )
    call mpiut__message_leader( comm, "Nloops_this_job",  &
                                      Params%get_integer( 'Nloops_this_job' ) )
    call mpiut__message_leader( comm, "Job.nloop_start", Job%nloop_start )
    call mpiut__message_leader( comm, "Job.nloop_end",   Job%nloop_end )
    call mpiut__message_leader( comm, "NPROC_X", NPROC_X )
    call mpiut__message_leader( comm, "NPROC_Y", NPROC_Y )
    call mpiut__message_leader( comm, "NPROC_Z", NPROC_Z )
    call mpiut__message_leader( comm, "NXPP", NXPP )
    call mpiut__message_leader( comm, "NYPP", NYPP )
    call mpiut__message_leader( comm, "NZPP", NZPP )
    call mpiut__message_leader( comm, "NX_GLOBAL", NX_GLOBAL )
    call mpiut__message_leader( comm, "NY_GLOBAL", NY_GLOBAL )
    call mpiut__message_leader( comm, "NZ_GLOBAL", NZ_GLOBAL )
    call mpiut__message_leader( comm, "XMIN", XMIN )
    call mpiut__message_leader( comm, "XMAX", XMAX )
    call mpiut__message_leader( comm, "YMIN", YMIN )
    call mpiut__message_leader( comm, "YMAX", YMAX )
    call mpiut__message_leader( comm, "ZMIN", ZMIN )
    call mpiut__message_leader( comm, "ZMAX", ZMAX )
    call mpiut__message_leader( comm, "Viscous_diffusivity",  &
                                      Params%get_double( 'Viscous_diffusivity' ) )
    call mpiut__message_leader( comm, "Thermal_diffusivity",  &
                                      Params%get_double( 'Thermal_diffusivity' ) )
  end subroutine iPrint

  function get_enstrophy( fluid )
    type(fluid_t) ,intent(in) :: fluid
    type(field__vector_t) :: vel !! 速度場（3D）
    type(field__vector_t) :: vor ! vorticity、渦度
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1) :: get_enstrophy ! 渦度の2乗
    call Solver%get_subfield(fluid,vel)
    vor = .curl. vel
    get_enstrophy = vor .dot. vor
  end function get_enstrophy

end program main_m
