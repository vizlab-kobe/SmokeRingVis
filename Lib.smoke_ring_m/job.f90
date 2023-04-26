!*******************************************************************
!> author: Akira Kageyama
!  license: MIT
!  date: 2020.01.22
!
!  シミュレーションジョブの制御
!
!@note 
!   配列演算を多用している。つまり一行で書かれている部分も
!   実際は3重do loopで書かれような大量の演算をしているところが
!   多い。このコードをOpenMP化する時には、そのような部分を
!   3重do loopに展開して書き直す必要がある。
!
module job_m
  use constants_m  ! 定数定義
  use ut_m         ! ユーティリティ
  implicit none    ! 暗黙の型宣言無効化。必須
  private ! このモジュール内の変数・ルーチン等はデフォルトで非公開
  public :: & ![variable]
            job__karte
  public :: & ![routines]
            job__finalize

  type, public :: job__karte_t
    ! ジョブの「健康状態」を記したカルテ用構造体
    character(len=20) :: state = "fine"  ! 初期は「健康」
  contains
    procedure :: set => job__karte_set   ! カルテの設定関数
  end type job__karte_t

  type(job__karte_t) :: job__karte ! カルテ


contains


  subroutine job__finalize(nloop)
    !! ジョブ終了時の後始末。実際には健康状態カルテに応じた
    !! メッセージを標準出力に書くだけ
    !!
    !! MPI化した場合、MPI_Finalizeをおくのはここがいいだろう。
    !!
    !! ut__messageの第一引数と第二引数はどちらも文字（列）
    !! 変数であるが、第一引数はシングルクォーテーションマーク、
    !! 第二引数はダブルクオーテーションマークで囲っている。これは
    !! コンパイラにとっては無意味。Fortranでは2つのクォーテーション
    !! マークは区別しない。
    integer(DI), intent(in) :: nloop !! ループカウンタ

    select case (trim(job__karte%state))
      case ("fine", "loop_max")
        call ut__message('#',"Successfully finished.") ! #で第2引数を囲む。
      case ("time_out")
        call ut__message('-',"Time out at nloop = ", nloop)
      case ("over_flow")
        call ut__message('%',"Overflow at nloop = ", nloop)
      case ("negative_anormaly")
        call ut__message('%',"Underflow at nloop = ",nloop)
      case default
        call ut__message('?',"Stopped at nloop = ",  nloop)
    end select
  end subroutine job__finalize


  subroutine job__karte_set(self, state_)
    !! ジョブカルテの設定終了時の後始末。実際には健康状態カルテに応じた
    !!
    !! 構造体のメンバー関数としてcallするときその
    !! 構造体変数そのものがselfとして自動的に引数にはいる。
    !! ここでの変数名はselfという名前でなくても構わない。
    class(job__karte_t), intent(out) :: self   !! ジョブカルテ
    character(len=*),    intent(in)  :: state_ !! 設定する状態

    select case (trim(state_))
      case ("fine")              ! 問題なく計算が進行している
        self%state = "fine"        
      case ("time_out")          ! ジョブの時間切れ
        self%state = "time_out" 
      case ("loop_max")          ! 設定されたループカウンタの最大値に到達した
        self%state = "loop_max" 
      case ("over_flow")         ! 計算途中にオーバーフローが発生した
        self%state = "over_flow" 
      case ("negative_anormaly") ! 密度など正でなければいけない量が負になった
        self%state = "negative_anormaly" 
      case default               ! そんなstateは想定していない
        call ut__fatal("<job__karte_set> case error.")
    end select
  end subroutine job__karte_set

end module job_m
