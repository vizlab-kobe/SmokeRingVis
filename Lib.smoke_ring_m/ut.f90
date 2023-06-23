!*******************************************************************
!> author: Akira Kageyama
!  license: MIT
!  date: 2020.01.22
!
!  ユーティリティライブラリ
!  
!  @note よく使う関数・ルーチンをまとめている。
! 
!  @note 冒頭でuse constants_mとしているが、
!        このモジュールで使うのはconstants
!        モジュールで定義された定数の中で
!        DR等のFortran関連の定数だけである。
!        それ以外のシミュレーション関係の定数は
!        使用しないこういう場合は単なる
!        use文にonlyというキーワードを
!        つけてDR等だけを使うことを明示的に
!        示すことができる。
!

module ut_m
  use constants_m  ! 定数定義
  implicit none    ! 暗黙の型宣言無効化。必須
  private ! このモジュール内の変数・ルーチン等はデフォルトで非公開
  public :: ut__assert,       &
            ut__fatal,        &
            ut__int_to_str3,  &
            ut__int_to_str7,  &
            ut__message

  interface ut__message
    !! 文字列（+ 数値）を標準出力に書き出すルーチンの多重定義
    !! 
    !! `message_decorated_なんとか` は飾り文字で囲む
    !! 
    !! `message_型名[_型名]` は文字列の後のその型の数値を同じ行に書く
     module procedure message_decorated_str,          &
                      message_decorated_str_dint,     &
                      message_decorated_str_sint,     &
                      message_str,                    &
                      message_str_double,             &
                      message_str_float,              &
                      message_str_dint,               &
                      message_str_dint_double,        &
                      message_str_dint_double_double, &
                      message_str_sint,               &
                      message_str_sint_double,        &
                      message_str_sint_double_double
  end interface



contains


  subroutine message_decorated_str(mark,string)
    !! 飾り文字でメーセージ文を囲む
    !!  
    !!   Usage:
    !!
    !!      call message_decorated_str('#',"This is a test.")
    !!
    !!   Output:
    !! 
    !!      ###################
    !!      # This is a test. #
    !!      ###################
    !!  
    character, intent(in)        :: mark   !! この文字でメーセージを囲む
    character(len=*), intent(in) :: string !! 出力メーセージ（文字列）
    integer(SI) :: len

    len = len_trim(string) + 4

    write(6,*) repeat(mark,len)
    write(6,*) mark//' '//trim(string)//' '//mark
    write(6,*) repeat(mark,len)
  end subroutine message_decorated_str


  subroutine message_decorated_str_dint(mark,string,int)
    !!  飾り文字でメーセージ文と倍精度整数を囲む
    !!
    !!  Usage:
    !!
    !!     call mess...('#','This is message at nloop = ', nloop)
    !!
    character, intent(in)        :: mark   !! この文字で全体を囲む
    character(len=*), intent(in) :: string !! メーセージ文字列
    integer(DI), intent(in)      :: int    !! 文字列の後に書く倍精度整数
    character(len=200) :: string_int

    write(string_int,'(a,i8)') string, int
    call message_decorated_str(mark, string_int)
  end subroutine message_decorated_str_dint


  subroutine message_decorated_str_sint(mark,string,int)
    !!  飾り文字でメーセージ文と単精度整数を囲む
    !!
    !!  Usage:
    !!
    !!     call mess...('#','This is message at nloop = ', nloop)
    character, intent(in)        :: mark    !! この文字で全体を囲む  
    character(len=*), intent(in) :: string  !! メーセージ文字列
    integer(SI), intent(in)      :: int     !! 文字列の後に書く単精度整数
    character(len=200) :: string_int

    write(string_int,'(a,i8)') string, int
    call message_decorated_str(mark, string_int)
  end subroutine message_decorated_str_sint


  subroutine message_str(string)
    !! 普通のprint文
    character(len=*), intent(in) :: string !! メーセージ文字列

    write(6,*) string
  end subroutine message_str


  subroutine message_str_double(string, double)
    !! 文字列 + 倍精度実数2つの出力
    character(len=*), intent(in) :: string   !! メーセージ文字列
    real(DR), intent(in)         :: double   !! 書き出される実数

    write(6,*) string, double
  end subroutine message_str_double


  subroutine message_str_float(string, float)
    !! 文字列 + 単精度実数2つの出力
    character(len=*), intent(in) :: string   !! メーセージ文字列
    real(SR), intent(in)         :: float    !! 書き出される実数

    write(6,*) string, float
  end subroutine message_str_float


  subroutine message_str_dint(string, int)
    !! 文字列 + 倍精度整数の出力
    character(len=*), intent(in) :: string  !! メーセージ文字列
    integer(DI), intent(in)      :: int     !! 書き出される整数

   write(6,*) string, int
  end subroutine message_str_dint


  subroutine message_str_sint(string, int)
    !! 文字列 + 単精度整数の出力
    character(len=*), intent(in) :: string  !! メーセージ文字列
    integer(SI), intent(in)      :: int     !! 書き出される整数

   write(6,*) string, int
  end subroutine message_str_sint


  subroutine message_str_dint_double(string, i1, d1)
    !! 文字列 + 倍精度整数 + 倍精度実数の出力
    character(len=*), intent(in) :: string  !! メーセージ文字列
    integer(DI), intent(in)      :: i1      !! 書き出される整数
    real(DR), intent(in)         :: d1      !! 書き出される実数

    write(6,*) string, i1, d1
  end subroutine message_str_Dint_double


  subroutine message_str_sint_double(string, i1, d1)
    !! 文字列 + 単精度整数 + 倍精度実数の出力
    character(len=*), intent(in) :: string  !! メーセージ文字列
    integer(SI), intent(in)      :: i1      !! 書き出される整数
    real(DR), intent(in)         :: d1      !! 書き出される実数

    write(6,*) string, i1, d1
  end subroutine message_str_sint_double


  subroutine message_str_dint_double_double(string, i1, d1, d2)
    !! 文字列 + 倍精度整数 + 倍精度実数 + 倍精度実数の出力
    character(len=*), intent(in) :: string  !! メーセージ文字列
    integer(DI), intent(in)      :: i1      !! 書き出される整数
    real(DR), intent(in)         :: d1, d2  !! 書き出される実数

    write(6,*) string, i1, d1, d2
  end subroutine message_str_Dint_double_double


  subroutine message_str_sint_double_double(string, i1, d1, d2)
    !! 文字列 + 単精度整数 + 倍精度実数 + 倍精度実数の出力
    character(len=*), intent(in) :: string  !! メーセージ文字列
    integer(SI), intent(in)      :: i1      !! 書き出される整数
    real(DR), intent(in)         :: d1, d2  !! 書き出される実数

    write(6,*) string, i1, d1, d2
  end subroutine message_str_sint_double_double



!
! Private  これより上が非公開の関数・ルーチン
!===================================================
! Public   これより下が公開する関数・ルーチン
!


  subroutine ut__assert(condition, last_will)
    !! アサーション
    logical, intent(in)          :: condition !! 必須条件
    character(len=*), intent(in) :: last_will !! 遺言

    if (.not.condition) then
       call ut__fatal(last_will)
    end if
  end subroutine ut__assert


  subroutine ut__fatal(last_will)
    !! 致命的エラー
    !!
    !!   遺言を書き出してプログラムを停止する
    !!
    character(len=*), intent(in) :: last_will
    call ut__message('!',last_will)
    stop 'Program stopped by ut__fatal.'
  end subroutine ut__fatal


  function ut__int_to_str3(i) result(str3)
    !! 整数を文字列（3文字固定）に変換する
    !!
    !!             e.g., i=1    --> str3="001"
    !!                   i=12   --> str3="012"
    !!                   i=123  --> str3="123"
    !!                   i=1234 --> str3="XXX"
    !! @bug 
    !!    i<0の時におかしくなる
    !!
    integer(SI), intent(in) :: i
    character(len=3) :: str3

    if ( i>999 ) then
      str3 = 'XXX'
    else
      write(str3,'(i3.3)') i
    end if
  end function ut__int_to_str3


  function ut__int_to_str7(i) result(str7)
    !! 整数を文字列（7文字固定）に変換する
    !!
    !!             e.g., i=1        --> str7="0000001"
    !!                   i=12       --> str7="0000012"
    !!                   i=123      --> str7="0000123"
    !!                   i=12345678 --> str7="XXXXXXX"
    !! @bug 
    !!    i<0の時におかしくなる
    !!
    integer(SI), intent(in) :: i
    character(len=7) :: str7

    if ( i>9999999 ) then
      str7 = 'XXXXXXX'
    else
      write(str7,'(i7.7)') i
    end if
  end function ut__int_to_str7

end module ut_m
