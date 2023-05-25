!!>
!   author: Akira Kageyama
!   date: 2023.05.05
! 
!   シミュレーションジョブの制御
! 
!   @note 
!      配列演算を多用している。つまり一行で書かれている部分も
!      実際は3重do loopで書かれような大量の演算をしているところが
!      多い。このコードをOpenMP化する時には、そのような部分を
!      3重do loopに展開して書き直す必要がある。
!!<
module job_m
  use constants_m  !! 定数定義
  use fluid_m      !! 流れ場データ
  use grid_m       !! 格子点情報
  use kutimer_m    !! 時間測定モジュール
  use parallel_m   !! MPI並列化
  use params_m     !! パラメータ
  use solver_m     !! ナビエ・ストークス方程式ソルバ
  use ut_m         !! ユーティリティ
  implicit none    !! 暗黙の型宣言無効化。必須
  private !! このモジュール内の変数・ルーチン等はデフォルトで非公開

  type :: job_t
    integer :: nloop       !! シミュレーションの繰り返しループ回数
    integer :: nloop_start !! このジョブの開始ループカウンタ
    integer :: nloop_end   !! このジョブの最終ループカウンタ
    character(len=20) :: karte = "fine"  ! カルテ。初期は「健康」
  contains
    procedure :: initialize => job__initialize
    procedure, nopass :: diagnosis => job__diagnosis
    procedure :: finalize => job__finalize
  end type job_t

  type(job_t), public :: Job


contains


  subroutine write_restart_data( nloop, time, fluid )
    integer, intent(in) :: nloop
    real(DR), intent(in) :: time
    type(fluid_t), intent(in) :: fluid
      !!>
!         リスタート用のデータファイルを書き出す。
      !!<

    character(len=3) :: str_proc_pos_x  !! プロセスのx位置。たとえば '001' 
    character(len=3) :: str_proc_pos_y  !!           y位置。たとえば '002' 
    character(len=3) :: str_proc_pos_z  !!           y位置。たとえば '003' 
                                   !! 上の3つを結合してファイル名とする。

    character(len=STR_LEN_MAX) :: jobname     !! e.g., 'may14a'
    character(len=STR_LEN_MAX) :: dir_restart !! e.g., 'data/resart'
    character(len=STR_LEN_MAX) :: file_info   !! e.g., 'may14a.info'
    character(len=STR_LEN_MAX) :: file_fluid  !! e.g., 'may14a_001_002_003.data'

    integer :: file_unit    !! ファイル番号

    dir_restart = trim(Params%get_string( 'Data_dir_name' )) // '/restart'
      !! リスタートデータが収められているディレクトリ
    jobname = Params%get_string( 'Job_name')
      !! ジョブの名前がファイル名に含まれている。
    file_info  = trim(dir_restart) // '/' // trim(jobname) // '.info'
      !! 時刻やプロセス数など全プロセスに共通する基本データを収めた1ファイル。
    file_fluid = trim(dir_restart) // '/' // trim(jobname) // '_' //  &
                     Parallel%pos%string // '.data'
      !! 流体データは各プロセス毎に担当する小領域を個別の名前で保存する。
  
    if ( Parallel%rank%me == 0 ) then
      call iStore_info
        !! 0番プロセスが基本データファイルを書き出す。
    end if
    call mpiut__barrier( Parallel%comm )
      !! 0番プロセスの書き出し終了を待つ。
      
    open(newunit=file_unit,file=trim(file_fluid),form='unformatted')
      write(file_unit) nloop, time
      write(file_unit) fluid%pressure
      write(file_unit) fluid%density
      write(file_unit) fluid%flux%x
      write(file_unit) fluid%flux%y
      write(file_unit) fluid%flux%z
    close(file_unit)

    call mpiut__barrier( Parallel%comm )

  contains

    subroutine iStore_info
      open(newunit=file_unit,file=trim(file_info),form='unformatted')
        write(file_unit) nloop, time
        write(file_unit) NPROC_X, NPROC_Y, NPROC_Z
        write(file_unit) NXPP, NYPP, NZPP
      close(file_unit)
    end subroutine iStore_info

  end subroutine write_restart_data


  subroutine read_restart_data( nloop, time,  fluid )
    integer, intent(out) :: nloop
    real(DR), intent(out) :: time
    type(fluid_t), intent(out) :: fluid
    !!>
!        リスタートデータを読み込む。nloop, time, fluid は上書きする。
    !!<

    character(len=3) :: str_proc_pos_x  !! プロセスのx位置。たとえば '001' 
    character(len=3) :: str_proc_pos_y  !!           y位置。たとえば '002' 
    character(len=3) :: str_proc_pos_z  !!           y位置。たとえば '003' 
                                   !! 上の3つを結合してファイル名とする。

    character(len=STR_LEN_MAX) :: jobname     !! e.g., 'may14a'
    character(len=STR_LEN_MAX) :: dir_restart !! e.g., 'data/resart'
    character(len=STR_LEN_MAX) :: file_info   !! e.g., 'may14a.info'
    character(len=STR_LEN_MAX) :: file_fluid  !! e.g., 'may14a_001_002_003.data'

    integer :: file_unit    !! ファイル番号
    integer :: nloop_check  !! 読み込んだファイルが正しい時刻であることを
    real(DR) :: time_check  !! 確認するための変数

    dir_restart = trim(Params%get_string( 'Data_dir_name' )) // '/restart'
      !! リスタートデータが収められているディレクトリ
    jobname = Params%get_string( 'Job_name')
      !! ジョブの名前がファイル名に含まれている。
    file_info  = trim(dir_restart) // '/' // trim(jobname) // '.info'
      !! 時刻やプロセス数など全プロセスに共通する基本データを収めた1ファイル。
    file_fluid = trim(dir_restart) // '/' // trim(jobname) // '_' //  &
                     Parallel%pos%string // '.data'
      !! 流体データは各プロセス毎に担当する小領域を個別の名前で保存する。
  
    nloop = 0      !! この後、allreduce_sum でデータ共有するために0に設定
    time = 0.0_DR  !! この後、allreduce_sum でデータ共有するために0に設定
    if ( Parallel%rank%me == 0 ) then
      call iRestore_and_check_info( file_info, nloop, time )
        !! 0番プロセスが基本データファイルを読み込む。
      call ut__message( "Job restart at nloop, time = ", nloop, time )
    end if
    call mpiut__allreduce_sum( Parallel%comm, nloop )
        !! 0番以外のプロセスはnloop=0なのでallreduce_sumはbroad castと同じ。
    call mpiut__allreduce_sum( Parallel%comm, time )
        !! 0番以外のプロセスはnloop=0なのでallreduce_sumはbroad castと同じ。
    open(newunit=file_unit,file=trim(file_fluid),form='unformatted')
      !! 各プロセスが担当領域の流体データを読み込む。
      read(file_unit) nloop_check, time_check   !! チェック用
      read(file_unit) fluid%pressure
      read(file_unit) fluid%density
      read(file_unit) fluid%flux%x
      read(file_unit) fluid%flux%y
      read(file_unit) fluid%flux%z
    close(file_unit)
    call mpiut__barrier( Parallel%comm )
      !! すべてのプロセスの読み込みが終わるまで待つ。

    call ut__assert( nloop_check == nloop,  &
                     'job_m(159): nloop error.' )
    call ut__assert( time_check == time,  &
                     'job_m(161): time error.' )
      !! これが一致していない場合はなにかがおかしい。

  contains

    subroutine iRestore_and_check_info( file_info, nloop, time )
      character(len=*), intent(in) :: file_info
      integer, intent(out) :: nloop
      real(DR), intent(out) :: time
        !!>
!           ディスクからデータを読み出す。
        !!<

      integer :: nloop_  !! 最後につけたアンダースコアは一時的な変数
      real(DR) :: time_  !! であることを強調している。
      integer :: nproc_x_, nproc_y_, nproc_z_
      integer :: nxpp_, nypp_, nzpp_
      
      open(newunit=file_unit,file=trim(file_info),form='unformatted')
        read(file_unit) nloop_, time_
        read(file_unit) nproc_x_, nproc_y_, nproc_z_
        read(file_unit) nxpp_, nypp_, nzpp_
      close(file_unit)

      nloop = nloop_
      time  = time_

      call ut__assert( NPROC_X == nproc_x_,  &
                     'job_m(189): nproc_x inconsistent.' )
      call ut__assert( NPROC_Y == nproc_y_,  &
                     'job_m(191): nproc_y inconsistent.' )
      call ut__assert( NPROC_Z == nproc_z_,  &
                     'job_m(193): nproc_z inconsistent.' )
      call ut__assert( NXPP == nxpp_,  &
                     'job_m(195): nxpp inconsistent.' )
      call ut__assert( NYPP == nypp_,  &
                     'job_m(197): nypp inconsistent.' )
      call ut__assert( NZPP == nzpp_,  &
                     'job_m(199): nzpp inconsistent.' )
    end subroutine iRestore_and_check_info

  end subroutine read_restart_data


  subroutine job__initialize( job, time, fluid )
    class(job_t), intent(out) :: job
    real(DR), intent(out) :: time
    type(fluid_t), intent(out) :: fluid

                                                        call kutimer__start('jobi  ')
    call Params%read                                   ;call kutimer__('jobi  ','params')
      !! パラメーターの読み込み。params.ef ファイルで定義された
      !! params_t型の変数 Params のメンバー関数 read の呼び出し。

    call Grid%initialize                               ;call kutimer__('jobi  ','grid i')
      !! gridモジュールの初期化。

    call Solver%initialize( fluid )                    ;call kutimer__('jobi  ','solv i')
      !! solverジュール（solver_m）の初期化。

    if ( Params%get_integer( 'Job_seq' ) == 0 ) then
      job%nloop = 0  !! ループカウンタの初期化。
      time = 0.0_DR  !! 時刻の初期化。単位は秒。
      !! fluid はSolver.initializeで初期化されている。
    else
      call read_restart_data( job%nloop, time, fluid ) ;call kutimer__('jobi  ','read r')
        !! 接続ジョブはディスクから初期データを読み出す。
    end if

    job%nloop_start = job%nloop
    job%nloop_end = job%nloop_start   &
                  + Params%get_integer( 'Nloops_this_job' )
                                                        call kutimer__end('jobi  ')
  end subroutine job__initialize


  subroutine job__finalize( job, nloop, time, fluid )
    class(job_t), intent(in) :: job
    integer, intent(in) :: nloop
    real(DR), intent(in) :: time
    type(fluid_t), intent(in) :: fluid

    !! ジョブ終了時の後始末。
    !! (1) 健康状態カルテに応じたメッセージを標準出力に書く
    !! (2) MPI並列化の終了処理

    if ( job%karte == "fine" .or. job%karte == "loop_max" ) then
      call write_restart_data( nloop, time, fluid )
    end if

    if ( Parallel%rank%me == 0 ) then
      select case (trim(job%karte))
        case ("fine","loop_max") !! このどちらかであれば、
          call ut__deco_message( "#","Successfully finished." ) 
                                 !! # で第2引数の文字列を囲む。
        case ("time out")
          call ut__deco_message( "-","Time out at nloop = ", nloop )
        case ("overflow")
          call ut__deco_message( "%","Overflow at nloop = ", nloop )
        case ("negative anormaly")
          call ut__deco_message( "%","Underflow at nloop = ",nloop )
        case default
          call ut__deco_message( "?","Stopped at nloop = ",  nloop )
      end select
    end if
  end subroutine job__finalize


  subroutine job__diagnosis( nloop, time, fluid )
    !! 流体の「健康状態」を診断する
    integer , intent(in) :: nloop  !! ループカウンタ
    real(DR), intent(in) :: time   !! シミュレーション時刻
    type(fluid_t), intent(in) :: fluid  !! 流体データ

    real(DR) :: max_vel_local, max_vel_global
    real(DR) :: total_mass_local, total_mass_global
    real(DR) :: total_energy_local, total_energy_global

    integer, parameter :: SKIP = 100
      !! このルーチンは結構計算負荷が高いので、
      !! 毎ステップではなく、SKIPステップごとに診断を実行する

    real(DR), parameter :: ANOMALOUSLY_LARGE = 1.e20_DR
      !! これよりも物理量が大きくなったら異常が生じたと判断する

    type(field__vector_t) :: vel
      !! 流れの速度場

    if ( mod(nloop,SKIP) /= 0 ) return

    if ( Job%karte /= "fine" ) return !! Already in error state.
      !! ジョブの健康状態がfine（つまり健康）以外の値に
      !! 設定する可能性があるのはここ以外にもある（例えばmain.f90の
      !! メインループでシミュレーションのループカウンタが最大値が達するなど）
      !! そのような場合、どうせこの後、ジョブの停止処理に入るので、
      !! これ以上計算を進めなくてもよい。

    if ( maxval(abs(fluid%flux%x)) > ANOMALOUSLY_LARGE ) then
      !! 質量フラックスのx成分が異常に大きい。
      call ut__message( "job_m(300): Massflux_x overflow." )
      Job%karte = "overflow" 
      return
        !! これ以上計算しても無駄（すぐに終了処理に入る）
    end if

    if ( maxval(abs(fluid%flux%y)) > ANOMALOUSLY_LARGE ) then
      call ut__message( "job_m(307): Massflux_y overflow." )
      Job%karte = "overflow"
      return
        !! これ以上計算しても無駄（すぐに終了処理に入る）
    end if

    if ( maxval(abs(fluid%flux%z)) > ANOMALOUSLY_LARGE ) then
      !! 質量フラックスのz成分が異常に大きい。
      call ut__message( "job_m(315): Massflux_z overflow." )
      Job%karte = "overflow"
      return
        !! これ以上計算しても無駄（すぐに終了処理に入る）
    end if

    if ( maxval(fluid%density) > ANOMALOUSLY_LARGE ) then
      !! 質量が異常に大きい
      call ut__message( "job_m(323): Density overflow." )
      Job%karte = "overflow"
      return
        !! これ以上計算しても無駄（すぐに終了処理に入る）
    end if

    if ( maxval(fluid%pressure) > ANOMALOUSLY_LARGE ) then
      !! 圧力が異常に大きい
      call ut__message( "job_m(331): Pressure overflow." )
      Job%karte = "overflow"
      return
        !! これ以上計算しても無駄（すぐに終了処理に入る）
    end if

    if ( minval(fluid%pressure) < 0.0_DR ) then
      !! 圧力が負になってしまっている
      call ut__message(" job_m(339): Negative pressure." )
      Job%karte = "negative anormaly"
      return
        !! これ以上計算しても無駄（すぐに終了処理に入る）
    end if

    if ( minval(fluid%density) < 0.0_DR ) then
      !! 密度が負になってしまっている
      call ut__message(" job_m(347): Negative density." )
      Job%karte = "negative anormaly"
      return
        !! これ以上計算しても無駄（すぐに終了処理に入るべし）
    end if

    call Solver%get_subfield( fluid, vel )
      !! 基本流れデータから副次的な速度場データを求める

    max_vel_local = sqrt(maxval(vel%x**2+vel%y**2+vel%z**2))
      !! 各プロセスでの速度の最大値（ベクトルの長さ）を計算する。
      !! わずか1行で書いているが、実際にはここにかなりの計算が
      !! 含まれている。vel.x**2という配列演算は実際には3重do loop
      !! であり、maxval関数はその引数の3次元配列をとっている。
      !! つまり全要素中の最大値をとっている。そして最後に
      !! sqrtをとって振幅（ベクトルの長さ）を計算している。
    max_vel_global = max_vel_local
      !! いったんコピー
    call mpiut__allreduce_max( Parallel%comm, max_vel_global )
    call mpiut__message( Parallel%comm,  &
                         '#max vel:',    &
                         nloop,          &
                         time,           &
                         max_vel_global )
      !! 全プロセスでの最大値を取得し、標準出力に書き出す。

    total_energy_local = .energyintegral. fluid 
      !! ここでもこの1行の実行にはかなりの演算（速度ベクトル場
      !! のx,y,z3成分の2乗和に質量密度を掛けたもの体積積分）が
      !! されていることに注意。
    total_energy_global = total_energy_local
    call mpiut__allreduce_sum( Parallel%comm, total_energy_global )
    call mpiut__message( Parallel%comm,      &
                         '#flow energy: ',   &
                         nloop,              &
                         time,               &
                         total_energy_global )

    total_mass_local = .scalarintegral. fluid%density
    total_mass_global = total_mass_local
    call mpiut__allreduce_sum( Parallel%comm, total_mass_global )
    call mpiut__message( Parallel%comm,    &
                         '#total mass: ',  &
                         nloop,            &
                         time,             &
                         total_mass_global )
      !! 上と同様。こちらのほうは単なる密度場の体積積分なので
      !! 演算量は少ないが、シミュレーション領域全体に渡る体積積分
      !! なので計算量は大きいことにかわりない。
  end subroutine job__diagnosis

end module job_m
