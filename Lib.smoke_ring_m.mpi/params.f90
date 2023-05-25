!!>
!   author: Akira Kageyama
!   date: 2023.05.05
! 
!   シミュレーションパラメータ
! 
!   @note 
!      パラメーターはnamelistを使ってファイルから読み込む
! 
!   @note 
!      先頭一文字だけが大文字になっている変数例えば Read_done などは
!      このモジュールを名前空間とする変数を意味する。つまりモジュール
!      外からはアクセスできないがモジュール内のルーチン関数からは
!      アクセスできるものである。
!      このようにスコープの広い変数を乱用するとバグの温床になるが
!      この程度の小さなプログラムであれば問題ないであろう。
! 
!   @note
!      ファイル番号10番をparams__readで使っている。
! 
!   @note
!     namelistファイルの内容を変更する場合は:
!       (1) このファイルの少し下の行にあるnamelist文を書き換える。
!       (2) params__readを書き換える。
!       (3) namelist__get_double, _integer等の対応する行も書き換える。
! 
!   @note
!     namelist文中のスラッシュで囲まれた名称（たとえば/simulation/）
!     や、namelist変数名（たとえばNloops_this_job）はnamelistファイル
!     の中での記述と対応していなければいけない。
!!<

module params_m
  use constants_m  !! 定数定義
  use ut_m         !! ユーティリティ
  use iso_fortran_env, only : output_unit
  implicit none    !! 暗黙の型宣言無効化。必須
  private !! このモジュール内の変数・ルーチン等はデフォルトで非公開

  logical, save :: Read_done = .false.   !! 読み込みが済んだか否か
  character(len=STR_LEN_MAX) :: Data_dir_name !! データ保存ディレクトリ名
  character(len=STR_LEN_MAX) :: Job_name      !! このジョブの名前
  integer :: Job_seq                     !! ジョブの連番。1以上なら接続ジョブ
  integer :: Nloops_this_job             !! このジョブの計算ループ数
  real(DR) :: Viscous_diffusivity        !! 粘性拡散率
  real(DR) :: Thermal_diffusivity        !! 熱拡散率

  namelist /data00/ Data_dir_name
  namelist /data01/ Job_name
  namelist /data02/ Job_seq
  namelist /data03/ Nloops_this_job
  namelist /data04/ Viscous_diffusivity
  namelist /data05/ Thermal_diffusivity

  type :: params_t
  contains
    procedure, nopass :: read => params__read
    procedure, nopass :: get_double => params__get_double
    procedure, nopass :: get_integer => params__get_integer
!   procedure, nopass :: get_logical => params__get_logical
    procedure, nopass :: get_string => params__get_string
  end type params_t

  type(params_t), public :: Params


contains


  function params__get_double( variable )
    character(len=*), intent(in) :: variable  !! 問い合わせ変数の名前
    real(DR) :: params__get_double            !! その値
    !! このモジュールの外からの問い合わせに応じてnamelistデータを返す。
    !! この関数は問い合わせ変数が倍精度浮動小数点数の場合。
    !! この関数の前にnamelist__readが呼ばれている必要がある。
    !! この点はassertで確認している。

    call ut__assert( Read_done, &
                     'params_m(__LINE):  Read params file first.' )

    select case (variable)
      case                 ('Thermal_diffusivity')    ! 熱拡散率
        params__get_double = Thermal_diffusivity
      case                 ('Viscous_diffusivity')    ! 粘性拡散率
        params__get_double = Viscous_diffusivity
      case default                                    ! 想定外
        call ut__message( '? arg = ', variable )
        call ut__fatal( 'params_m(88): case error.' )
    end select
  end function params__get_double


  function params__get_integer( variable )
    character(len=*), intent(in) :: variable    !! 問い合わせ変数の名前
    integer :: params__get_integer          !! その値
    !! このモジュールの外からの問い合わせに応じてnamelistデータを返す。
    !! この関数は問い合わせ変数が整数の場合。
    !! この関数の前にnamelist__readが呼ばれている必要がある。
    !! この点はassertで確認している。

    call ut__assert( Read_done, &
                     'params_m(102): Read params file first.' )

    select case (variable)
      case                  ('Nloops_this_job')  ! シミュレーションジョブ
        params__get_integer = Nloops_this_job    ! の実行最大ループ数
      case                  ('Job_seq')          ! Sequential number
        params__get_integer = Job_seq            ! of this job series.
      case default                               ! 想定外
        call ut__message( '? arg = ', variable ) 
        call ut__fatal( 'params_m(111): case error.' )
    end select
  end function params__get_integer

!!>
!  function params__get_logical( variable )
!    character(len=*), intent(in) :: variable  !! 問い合わせ変数の名前
!    logical :: params__get_logical            !! その値
!    !! このモジュールの外からの問い合わせに応じてnamelistデータを返す。
!    !! この関数は問い合わせ変数が論理値の場合。
!    !! この関数の前にnamelist__readが呼ばれている必要がある。
!    !! この点はassertで確認している。
!
!    call ut__assert( Read_done, &
!                     'params_m(125): Read params file first.' )
!
!    select case (variable)
!      case default                     ! 想定外
!        call ut__message( '? arg = ', variable )
!        call ut__fatal( 'params_m(130): case error.' )
!    end select
!  end function params__get_logical
!!<

  subroutine params__read
    !! namelistファイルをディスクから読み込む。
    !! ファイル名はコマンド第一引数。
    !!
    !! namelistデータファイルの内容を変更する場合は
    !! 以下のread文も適宜変更する。
    !!
    character(len=STR_LEN_MAX) :: params_file
    integer :: file_unit

    call ut__assert( command_argument_count()==1, &
                     "params_m(146): Usage: smoke_ring param_file")
    call get_command_argument(1,params_file)

    !*******<params_file のサンプル>************
    ! &data00        Data_dir_name = '../data' /
    ! &data01             Job_name = 'may13a'  /
    ! &data02              Job_seq = 0         /
    ! &data03      Nloops_this_job = 20000     /
    ! &data04  Viscous_diffusivity = 5.0e-3    /
    ! &data05  Thermal_diffusivity = 5.0e-3    /
    !*******</params_file のサンプル>***********

    open(newunit=file_unit,file=trim(params_file))
      read(file_unit,nml=data00)
      read(file_unit,nml=data01)
      read(file_unit,nml=data02)
      read(file_unit,nml=data03)
      read(file_unit,nml=data04)
      read(file_unit,nml=data05)
    close(file_unit)

    ! write(output_unit,nml=data00)
    ! write(output_unit,nml=data01)
    ! write(output_unit,nml=data02)
    ! write(output_unit,nml=data03)
    ! write(output_unit,nml=data04)
    ! write(output_unit,nml=data05)

    Read_done = .true.
  end subroutine params__read


  function params__get_string( variable )
    character(len=*), intent(in) :: variable  !! 問い合わせ変数の名前
    character(len=STR_LEN_MAX) :: params__get_string  !! その値
    !! このモジュールの外からの問い合わせに応じてnamelistデータを返す。
    !! この関数は問い合わせ変数が文字列の場合。
    !! この関数の前にnamelist__readが呼ばれている必要がある。
    !! この点はassertで確認している。

    call ut__assert( Read_done, &
                     'params_m(187): Read params file first.' )

    select case (variable)
      case                 ('Job_name')
        params__get_string = Job_name            !! このジョブの名前
      case                 ('Data_dir_name')
        params__get_string = Data_dir_name       !! データ保存ディレクトリ名
      case default
        call ut__message( '? arg = ', variable ) !! 想定外
        call ut__fatal( 'params_m(196): case error.' )
    end select
  end function params__get_string

end module params_m
