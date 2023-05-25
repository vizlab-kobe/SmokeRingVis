!!>
!   author: Akira Kageyama
!   date: 2023.05.05
! 
!   ユーティリティライブラリ
!   
!   @note よく使う関数・ルーチンをまとめる。
!  
!   @note 実際には使わないルーチンも多数あり。
!!<

module ut_m
  use constants_m, only : SI, DI, SR, DR, NIL  ! 定数定義
  implicit none
  private
  public :: & !< routines >
            ut__appear_pos,               &
            ut__assert,                   &
            ut__atan,                     &
            ut__date_and_time,            &
            ut__deco_message,             &
            ut__fatal,                    &
            ut__gets,                     &
            ut__i2c1,                     &
            ut__i2c3,                     &
            ut__i2c4,                     &
            ut__i2c7,                     &
            ut__int_to_str3,              &
            ut__int_to_str4,              &
            ut__int_to_str5,              &
            ut__int_to_str6,              &
            ut__int_to_str8,              &
            ut__int_to_str10,             &
            ut__real_to_str13,            &
            ut__make_queue,               &
            ut__message,                  &
            ut__message_leader,           &
            ut__positive_int_to_str9,     &
            ut__positive_real_to_str9,    &
            ut__real_to_str10,            &
            ut__sizeof,                   &
            ut__sleep,                    &
            ut__smooth_step,              &
            ut__step,                     &
            ut__zeroset

  interface ut__message
    !! 文字列（+ 数値）を標準出力に書き出すルーチンの多重定義
    !! 
    !! `message_decorated_なんとか` は飾り文字で囲む
    !! 
    !! `message_型名[_型名]` は文字列の後のその型の数値を同じ行に書く
     module procedure message_str,                &
                      message_str_dr,             &
                      message_str_dr_str,         &
                      message_str_si,             &
                      message_str_di,             &
                      message_str_si_si,          &
                      message_str_si_di,          &
                      message_str_di_si,          &
                      message_str_di_di,          &
                      message_str_si_si_si,       &
                      message_str_si_si_si_si,    &
                      message_str_si_si_si_si_si, &
                      message_str_si_si_dr,       &
                      message_str_di_si_dr,       &
                      message_str_si_dr,          &
                      message_str_di_dr,          &
                      message_str_si_dr_dr,       &
                      message_str_di_dr_dr,       &
                      message_str_si_sr_sr,       &
                      message_str_di_sr_sr,       &
                      message_str_si_str,         &
                      message_str_si_str_si,      &
                      message_str_di_str_si,      &
                      message_str_si_str_dr,      &
                      message_str_di_str_dr,      &
                      message_str_logical,        &
                      message_str_logical_str,    &
                      message_str_str
  end interface

  interface ut__message_leader
    module procedure message_leader_si,          &
                     message_leader_di,          &
                     message_leader_dr,          &
                     message_leader_sr,          &
                     message_leader_logical,     &
                     message_leader_str
  end interface

  interface ut__deco_message
    module procedure message_decorated_str,      &
                     message_decorated_str_si,   &
                     message_decorated_str_di
  end interface

  interface ut__real_to_str10
    module procedure real_to_str10_dr,  &
                     real_to_str10_sr
  end interface

  interface ut__real_to_str13
    module procedure real_to_str13_dr,  &
                     real_to_str13_sr
  end interface

  interface ut__positive_real_to_str9
    module procedure positive_dr_to_str9,   &
                     positive_sr_to_str9
  end interface

  interface ut__int_to_str3
    module procedure di_to_str3_with_sign,  &
                     si_to_str3_with_sign
  end interface

  interface ut__int_to_str4
    module procedure di_to_str4_with_sign,  &
                     si_to_str4_with_sign
  end interface

  interface ut__int_to_str5
    module procedure di_to_str5_with_sign,  &
                     si_to_str5_with_sign
  end interface

  interface ut__int_to_str6
    module procedure di_to_str6_with_sign,  &
                     si_to_str6_with_sign
  end interface

  interface ut__int_to_str8
    module procedure di_to_str8_with_sign,  &
                     si_to_str8_with_sign
  end interface

  interface ut__int_to_str10
    module procedure di_to_str10_with_sign,  &
                     si_to_str10_with_sign
  end interface

  interface ut__positive_int_to_str9
    module procedure positive_di_to_str9,  &
                     positive_si_to_str9
  end interface

  interface ut__sizeof
    module procedure sizeof_0d_di,    &
                     sizeof_0d_si,    &
                     sizeof_0d_dr,    &
                     sizeof_0d_sr
  end interface                 

  interface ut__zeroset
     module procedure zeroset_dr01,        &
                      zeroset_dr02,        &
                      zeroset_dr03,        &
                      zeroset_dr04,        &
                      zeroset_dr_array01,  &
                      zeroset_dr_array02,  &
                      zeroset_dr_array03,  &
                      zeroset_dr_array04,  &
                      zeroset_sr01,        &
                      zeroset_sr02,        &
                      zeroset_sr03,        &
                      zeroset_sr04,        &
                      zeroset_sr_array01,  &
                      zeroset_sr_array02,  &
                      zeroset_sr_array03,  &
                      zeroset_sr_array04
  end interface

  integer(SI), parameter :: UT__STRLEN_MAX = 5000   ! any long enough int.

contains



  function di_to_str3_with_sign( i ) result(str)
    integer(DI), intent(in) :: i
    character(len=3)      :: str
    !!>
!       整数を文字列（3文字符号付き）に変換する
!              i =   10 --> str="+10"
!              i = -123 --> str="-XX"
    !!<
    if ( i < 0 ) then
      str(1:1) = '-'
    else
      str(1:1) = '+'
    end if
    if ( abs(i) > 99 ) then
      str(2:3) = 'XX'
    else
      write(str(2:3),'(i2.2)') abs(i)
    end if
  end function di_to_str3_with_sign


  function si_to_str3_with_sign( i ) result(str)
    integer(SI), intent(in) :: i
    character(len=3)      :: str
    !!>
!      Convert an integer into 3 characters with sign.
!              i =   10 --> str="+00"
!              i = -123 --> str="-NN"
    !!<
    if ( i < 0 ) then
      str(1:1) = '-'
    else
      str(1:1) = '+'
    end if
    if ( abs(i) > 99 ) then
      str(2:3) = 'XX'
    else
      write(str(2:3),'(i2.2)') abs(i)
    end if
  end function si_to_str3_with_sign


  function di_to_str4_with_sign( i ) result(str)
    integer(DI), intent(in) :: i
    character(len=4) :: str
    !!>
!      Convert an integer into 4 characters with sign.
!              i =   10 --> str="+010"
!              i = -123 --> str="-123"
    !!<
    if ( i < 0 ) then
      str(1:1) = '-'
    else
      str(1:1) = '+'
    end if
    if ( abs(i) > 999 ) then
      str(2:4) = 'XXX'
    else
      write(str(2:4),'(i3.3)') abs(i)
    end if
  end function di_to_str4_with_sign


  function si_to_str4_with_sign( i ) result(str)
    integer(SI), intent(in) :: i
    character(len=4)      :: str
    !!>
!      Convert an integer into 4 characters with sign.
!              i =   10 --> str="+010"
!              i = -123 --> str="-123"
    !!<
    if ( i < 0 ) then
      str(1:1) = '-'
    else
      str(1:1) = '+'
    end if
    if ( abs(i) > 999 ) then
      str(2:4) = 'XXX'
    else
      write(str(2:4),'(i3.3)') abs(i)
    end if
  end function si_to_str4_with_sign


  function di_to_str5_with_sign( i ) result(str)
    integer(DI), intent(in) :: i
    character(len=5)      :: str
    !!>
!      Convert an integer into 5 characters with sign.
!              i =   10 --> str="+0010"
!              i = -123 --> str="-0123"
    !!<
    if ( i < 0 ) then
      str(1:1) = '-'
    else
      str(1:1) = '+'
    end if
    if ( abs(i) > 9999 ) then
      str(2:5) = 'XXXX'
    else
      write(str(2:5),'(i4.4)') abs(i)
    end if
  end function di_to_str5_with_sign


  function si_to_str5_with_sign( i ) result(str)
    integer(SI), intent(in) :: i
    character(len=5)      :: str
    !!>
!      Convert an integer into 5 characters with sign.
!              i =   10 --> str="+0010"
!              i = -123 --> str="-0123"
    !!<
    if ( i < 0 ) then
      str(1:1) = '-'
    else
      str(1:1) = '+'
    end if
    if ( abs(i) > 9999 ) then
      str(2:5) = 'XXXX'
    else
      write(str(2:5),'(i4.4)') abs(i)
    end if
  end function si_to_str5_with_sign


  function di_to_str6_with_sign( i ) result(str)
    integer(DI), intent(in) :: i
    character(len=6)      :: str
    !!>
!      Convert an integer into 6 characters with sign.
!              i =   10 --> str="+00010"
!              i = -123 --> str="-00123"
    !!<
    if ( i < 0 ) then
      str(1:1) = '-'
    else
      str(1:1) = '+'
    end if
    if ( abs(i) > 99999 ) then
      str(2:6) = 'XXXXX'
    else
      write(str(2:6),'(i5.5)') abs(i)
    end if
  end function di_to_str6_with_sign


  function si_to_str6_with_sign( i ) result(str)
    integer(SI), intent(in) :: i
    character(len=6)      :: str
    !!>
!      Convert an integer into 6 characters with sign.
!              i =   10 --> str="+00010"
!              i = -123 --> str="-00123"
    !!<
    if ( i < 0 ) then
      str(1:1) = '-'
    else
      str(1:1) = '+'
    end if
    if ( abs(i) > 99999 ) then
      str(2:6) = 'XXXXX'
    else
      write(str(2:6),'(i5.5)') abs(i)
    end if
  end function si_to_str6_with_sign


  function di_to_str8_with_sign( i ) result(str)
    integer(DI), intent(in) :: i
    character(len=8)      :: str
    !!>
!      Convert an integer into 8 characters with sign.
!              i =   10 --> str="+0000010"
!              i = -123 --> str="-0000123"
    !!<
    if ( i < 0 ) then
      str(1:1) = '-'
    else
      str(1:1) = '+'
    end if
    if ( abs(i) > 9999999 ) then
      str(2:8) = 'XXXXXXX'
    else
      write(str(2:8),'(i7.7)') abs(i)
    end if
  end function di_to_str8_with_sign


  function si_to_str8_with_sign( i ) result(str)
    integer(SI), intent(in) :: i
    character(len=8)      :: str
    !!>
!      Convert an integer into 8 characters with sign.
!              i =   10 --> str="+0000010"
!              i = -123 --> str="-0000123"
    !!<
    if ( i < 0 ) then
      str(1:1) = '-'
    else
      str(1:1) = '+'
    end if
    if ( abs(i) > 9999999 ) then
      str(2:8) = 'XXXXXXX'
    else
      write(str(2:8),'(i7.7)') abs(i)
    end if
  end function si_to_str8_with_sign


  function di_to_str10_with_sign( i ) result(str)
    integer(DI), intent(in) :: i
    character(len=10)     :: str
    !!>
!      Convert an integer into 10 characters with sign.
!              i =   10 --> str="+000000010"
!              i = -123 --> str="-000000123"
    !!<
    if ( i < 0 ) then
      str(1:1) = '-'
    else
      str(1:1) = '+'
    end if
    if ( abs(i) > 999999999 ) then
      str(2:10) = 'XXXXXXXXX'
    else
      write(str(2:10),'(i9.9)') abs(i)
    end if
  end function di_to_str10_with_sign


  function si_to_str10_with_sign( i ) result(str)
    integer(SI), intent(in) :: i
    character(len=10) :: str
    !!>
!      Convert an integer into 10 characters with sign.
!              i =   10 --> str="+000000010"
!              i = -123 --> str="-000000123"
    !!<
    if ( i < 0 ) then
      str(1:1) = '-'
    else
      str(1:1) = '+'
    end if
    if ( abs(i) > 999999999 ) then
      str(2:10) = 'XXXXXXXXX'
    else
      write(str(2:10),'(i9.9)') abs(i)
    end if
  end function si_to_str10_with_sign


  subroutine message_decorated_str( mark, string )
    character(len=1), intent(in) :: mark    !! この文字でメーセージを囲
    character(len=*), intent(in) :: string  !! 出力メーセージ（文字列
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
    integer(SI) :: len

    len = len_trim(string) + 4

    print *,  repeat(mark,len)
    print *,  mark//' '//trim(string)//' '//mark
    print *,  repeat(mark,len)
  end subroutine message_decorated_str


  subroutine message_decorated_str_si( mark, string, int )
    character(len=1), intent(in) :: mark    !! この文字で全体を囲む 
    character(len=*), intent(in) :: string  !! メーセージ文字列
    integer(SI), intent(in) :: int     !! 文字列の後に書く整数 
      !!>
!        Usage:
!           call mess...( '#', 'This is message at nloop = ', nloop )
      !!<
    character(len=200) :: string_int

    write(string_int,'(a,i8)') string, int
    call message_decorated_str( mark, string_int )
  end subroutine message_decorated_str_si


  subroutine message_decorated_str_di( mark, string, int )
    character(len=1), intent(in) :: mark
    character(len=*), intent(in) :: string
    integer(DI), intent(in) :: int
     !!>
!       Usage:
!          call mess...( '#', 'This is message at nloop = ', nloop )
     !!<
    character(len=200) :: string_int
    write(string_int,'(a,i14)') string, int
    call message_decorated_str( mark, string_int )
  end subroutine message_decorated_str_di


  subroutine message_leader_si( string, i01 )
    character(len=*), intent(in) :: string
    integer(SI), intent(in) :: i01

    integer(SI), parameter :: TOTAL_LENGTH = 60

    character(len=12) :: string_for_i01
    character(len=TOTAL_LENGTH) :: line

    line = repeat('_', TOTAL_LENGTH)

    write(string_for_i01,'(a1,i0)') ' ', i01 ! put a space in front of i01
    line(1:len_trim(string)) = trim(string)
    line(TOTAL_LENGTH-len_trim(string_for_i01):TOTAL_LENGTH)    &
       = trim(string_for_i01)
    print *,  line
  end subroutine message_leader_si


  subroutine message_leader_di( string, i01 )
    character(len=*), intent(in) :: string
    integer(DI), intent(in) :: i01

    integer(SI), parameter :: TOTAL_LENGTH = 60
    character(len=20) :: string_for_i01
    character(len=TOTAL_LENGTH) :: line

    line = repeat('_', TOTAL_LENGTH)

    write(string_for_i01,'(a1,i0)') ' ', i01 ! put a space in front of i01
    line(1:len_trim(string)) = trim(string)
    line(TOTAL_LENGTH-len_trim(string_for_i01):TOTAL_LENGTH)    &
       = trim(string_for_i01)
    print *,  line
  end subroutine message_leader_di


  subroutine message_leader_logical( string, bool )
    character(len=*), intent(in) :: string
    logical, intent(in) :: bool

    integer(SI), parameter :: TOTAL_LENGTH = 60
    character(len=TOTAL_LENGTH) :: line

    line = repeat('_', TOTAL_LENGTH)

    if ( bool ) then
      line(TOTAL_LENGTH-6:TOTAL_LENGTH) = ".true."
    else
      line(TOTAL_LENGTH-7:TOTAL_LENGTH) = ".false."
    end if

    line(1:len_trim(string)) = trim(string)

    print *,  line
  end subroutine message_leader_logical


  subroutine message_leader_dr( string, d01 )
    character(len=*), intent(in) :: string
    real(DR)   , intent(in) :: d01

    integer(SI), parameter :: TOTAL_LENGTH = 60
    character(len=22) :: string_for_value
    character(len=TOTAL_LENGTH) :: line

    line = repeat('_',TOTAL_LENGTH)

    write(string_for_value,'(1pe22.15)') d01
    line(1:len_trim(string)) = trim(string)
    line(TOTAL_LENGTH-22:TOTAL_LENGTH) = string_for_value
    print *,  line
  end subroutine message_leader_dr


  subroutine message_leader_sr( string, f01 )
    character(len=*), intent(in) :: string
    real(SR)   , intent(in) :: f01

    integer(SI), parameter :: TOTAL_LENGTH = 60
    character(len=12) :: string_for_value
    character(len=TOTAL_LENGTH) :: line

    line = repeat('_',TOTAL_LENGTH)

    write(string_for_value,'(1pe12.5)') f01
    line(1:len_trim(string)) = trim(string)
    line(TOTAL_LENGTH-12:TOTAL_LENGTH) = string_for_value
    print *,  line
  end subroutine message_leader_sr


  subroutine message_leader_str( string, s01 )
    character(len=*), intent(in) :: string
    character(len=*), intent(in) :: s01

    integer(SI), parameter :: TOTAL_LENGTH = 60
    character(len=len_trim(s01)+1) :: s01_
    character(len=TOTAL_LENGTH) :: line
    line = repeat('_',TOTAL_LENGTH)
    s01_ = ' '//s01  ! put a blank in front of s01.

    line(1:len_trim(string)) = trim(string)
    line(TOTAL_LENGTH-len_trim(s01_):TOTAL_LENGTH) = s01_
    print *,  line
  end subroutine message_leader_str


  subroutine message_str( string )
    character(len=*), intent(in) :: string

    print *,  string
  end subroutine message_str


  subroutine message_str_dr( string, double )
    character(len=*), intent(in) :: string
    real(DR)   , intent(in) :: double

    print *,  string, double
  end subroutine message_str_dr


  subroutine message_str_dr_str( str01, double, str02 )
    character(len=*), intent(in) :: str01, str02
    real(DR)   , intent(in) :: double

    print *,  str01, double, str02
  end subroutine message_str_dr_str



  subroutine message_str_str( string01, string02 )
    character(len=*), intent(in) :: string01
    character(len=*), intent(in) :: string02

    print *,  string01, string02
  end subroutine message_str_str


  subroutine message_str_si( string, int )
    character(len=*), intent(in) :: string
    integer(SI), intent(in) :: int

    print *,  string, int
  end subroutine message_str_si


  subroutine message_str_di( string, int )
    character(len=*), intent(in) :: string
    integer(DI), intent(in) :: int

    print *,  string, int
  end subroutine message_str_di


  subroutine message_str_si_si( string, i1, i2 )
    character(len=*), intent(in) :: string
    integer(SI), intent(in) :: i1, i2

    print *,  string, i1, i2
  end subroutine message_str_si_si


  subroutine message_str_si_di( string, i1, i2 )
    character(len=*), intent(in) :: string
    integer(SI), intent(in) :: i1
    integer(DI), intent(in) :: i2

    print *,  string, i1, i2
  end subroutine message_str_si_di


  subroutine message_str_di_si( string, i1, i2 )
    character(len=*), intent(in) :: string
    integer(DI), intent(in) :: i1
    integer(SI), intent(in) :: i2

    print *,  string, i1, i2
  end subroutine message_str_di_si


  subroutine message_str_di_di( string, i1, i2 )
    character(len=*), intent(in) :: string
    integer(DI), intent(in) :: i1, i2

    print *,  string, i1, i2
  end subroutine message_str_di_di


  subroutine message_str_si_si_si( string, i1, i2, i3 )
    character(len=*), intent(in) :: string
    integer(SI), intent(in) :: i1, i2, i3

    print *,  string, i1, i2, i3
  end subroutine message_str_si_si_si


  subroutine message_str_si_si_si_si( string,  &
                                      i1, i2, i3, i4 )
    character(len=*), intent(in) :: string
    integer(SI), intent(in) :: i1, i2, i3, i4

    print *,  string, i1, i2, i3, i4
  end subroutine message_str_si_si_si_si


  subroutine message_str_si_si_si_si_si( string,  &
                                         i1, i2, i3, i4, i5 )
    character(len=*), intent(in) :: string
    integer(SI), intent(in) :: i1, i2, i3, i4, i5

    print *,  string, i1, i2, i3, i4, i5
  end subroutine message_str_si_si_si_si_si


  subroutine message_str_si_si_dr( string, i1, i2, d1 )
    character(len=*), intent(in) :: string
    integer(SI), intent(in) :: i1, i2
    real(DR)   , intent(in) :: d1

    print *,  string, i1, i2, d1
  end subroutine message_str_si_si_dr


  subroutine message_str_di_si_dr( string, i1, i2, d1 )
    character(len=*), intent(in) :: string
    integer(DI), intent(in) :: i1
    integer(SI), intent(in) :: i2
    real(DR)   , intent(in) :: d1

    print *,  string, i1, i2, d1
  end subroutine message_str_di_si_dr



  subroutine message_str_si_dr( string, i1, d1 )
    character(len=*), intent(in) :: string
    integer(SI), intent(in) :: i1
    real(DR)   , intent(in) :: d1

    print *,  string, i1, d1
  end subroutine message_str_si_dr


  subroutine message_str_di_dr( string, i1, d1 )
    character(len=*), intent(in) :: string
    integer(DI), intent(in) :: i1
    real(DR)   , intent(in) :: d1

    print *,  string, i1, d1
  end subroutine message_str_di_dr


  subroutine message_str_si_dr_dr( string, i1, d1, d2 )
    character(len=*), intent(in) :: string
    integer(SI), intent(in) :: i1
    real(DR)   , intent(in) :: d1, d2

    print *,  string, i1, d1, d2
  end subroutine message_str_si_dr_dr


  subroutine message_str_di_dr_dr( string, i1, d1, d2 )
    character(len=*), intent(in) :: string
    integer(DI), intent(in) :: i1
    real(DR)   , intent(in) :: d1, d2

    print *,  string, i1, d1, d2
  end subroutine message_str_di_dr_dr


  subroutine message_str_si_sr_sr( string, i1, f1, f2 )
    character(len=*), intent(in) :: string
    integer(SI), intent(in) :: i1
    real(SR)   , intent(in) :: f1, f2
    print *,  string, i1, f1, f2
  end subroutine message_str_si_sr_sr


  subroutine message_str_di_sr_sr( string, i1, f1, f2 )
    character(len=*), intent(in) :: string
    integer(DI), intent(in) :: i1
    real(SR)   , intent(in) :: f1, f2
    print *,  string, i1, f1, f2
  end subroutine message_str_di_sr_sr


  subroutine message_str_si_str( str1, i1, str2 )
    character(len=*), intent(in) :: str1, str2
    integer(SI), intent(in) :: i1
    print *,  str1, i1, str2
  end subroutine message_str_si_str


  subroutine message_str_si_str_si( str1, i1, str2, i2 )
    character(len=*), intent(in) :: str1, str2
    integer(SI), intent(in) :: i1, i2
    print *,  str1, i1, str2, i2
  end subroutine message_str_si_str_si


  subroutine message_str_di_str_si( str1, i1, str2, i2 )
    character(len=*), intent(in) :: str1, str2
    integer(DI), intent(in) :: i1
    integer(SI), intent(in) :: i2

    print *,  str1, i1, str2, i2
  end subroutine message_str_di_str_si


  subroutine message_str_si_str_dr( str1, i1, str2, d1 )
    character(len=*), intent(in) :: str1, str2
    integer(SI), intent(in) :: i1
    real(DR)   , intent(in) :: d1
    print *,  str1, i1, str2, d1
  end subroutine message_str_si_str_dr


  subroutine message_str_di_str_dr( str1, i1, str2, d1 )
    character(len=*), intent(in) :: str1, str2
    integer(DI), intent(in) :: i1
    real(DR)   , intent(in) :: d1

    print *,  str1, i1, str2, d1
  end subroutine message_str_di_str_dr

  subroutine message_str_logical( str1, logical )
    character(len=*), intent(in) :: str1
    logical    , intent(in) :: logical
    print *,  str1, logical
  end subroutine message_str_logical

  subroutine message_str_logical_str( str1, logical, str2 )
    character(len=*), intent(in) :: str1, str2
    logical    , intent(in) :: logical
    print *,  str1, logical, str2
  end subroutine message_str_logical_str


  function positive_di_to_str9( i ) result(str)
    integer(DI), intent(in) :: i
    character(len=9)      :: str

    !!>
!      Convert an integer into 9 characters with sign.
!              i =  123 --> str="000000123"
    !!<
    character(len=10) :: str10
    if ( i < 0 ) then
      str = '---------'
    else if ( abs(i) > 999999999 ) then
      str = 'XXXXXXXXX'
    else
      str10 = ut__int_to_str10( i )
      str = str10(2:10)
    end if
  end function positive_di_to_str9


  function positive_si_to_str9( i ) result(str)
    integer(SI), intent(in) :: i
    character(len=9)      :: str

    !!>
!      Convert an integer into 9 characters with sign.
!              i =  123 --> str="000000123"
    !!<
    character(len=10) :: str10
    if ( i < 0 ) then
      str = '---------'
    else if ( abs(i) > 999999999 ) then
      str = 'XXXXXXXXX'
    else
      str10 = ut__int_to_str10(i)
      str = str10(2:10)
    end if
  end function positive_si_to_str9


  function positive_dr_to_str9( d ) result(str)
    real(DR), intent(in) :: d
    character(len=9)   :: str
    !!>
!      double to character string, e.g., d=0.123456 ==> str="1.234E-01"
    !!<
    character(len=10) :: str10
    if ( d < 0.0_DR ) then
      str = '---------'
    else
      str10 = ut__real_to_str10( d )
      str = str10(2:10)
    end if
  end function positive_dr_to_str9


  function positive_sr_to_str9( f ) result(str)
    real(SR), intent(in) :: f
    character(len=9)   :: str
    !!>
!      double to character string, e.g., f=0.123456 ==> str="1.234E-01"
    !!<
    character(len=10) :: str10
    if ( f < 0.0_SR ) then
      str = '---------'
    else
      str10 = ut__real_to_str10( f )
      str = str10(2:10)
    end if
  end function positive_sr_to_str9


  function real_to_str10_dr( d ) result(str)
    real(DR), intent(in) :: d
    character(len=10)  :: str
    !!>
!      double to character string, e.g., d=0.123456 ==> str="+1.235E1"
    !!<
    write(str,'(1pe10.3)') d
    if(str(1:1)==' ') str(1:1)='+'
  end function real_to_str10_dr


  function real_to_str10_sr( f ) result(str)
    real(SR), intent(in) :: f
    character(len=10)  :: str
    !!>
!      double to character string, e.g., f=0.123456 ==> str="+1.235E1"
    !!<
    write(str,'(1pe10.3)') f
    if(str(1:1)==' ') str(1:1)='+'
  end function real_to_str10_sr


  function real_to_str13_dr( d ) result(str)
    real(DR), intent(in) :: d
    character(len=13)  :: str
    !!>
!      double to character string, e.g., d=0.123456789 ==> str="+1.234567E-01"
    !!<
    write(str,'(1pe13.6)') d
    if(str(1:1)==' ') str(1:1)='+'
  end function real_to_str13_dr


  function real_to_str13_sr( f ) result(str)
    real(SR), intent(in) :: f
    character(len=13)  :: str
    !!>
!      double to character string, e.g., f=0.123456789 ==> str="+1.234567E-01"
    !!<
    write(str,'(1pe13.6)') f
    if(str(1:1)==' ') str(1:1)='+'
  end function real_to_str13_sr


  function sizeof_0d_di( i ) result(size)
    integer(DI), intent(in) :: i
    integer(SI) :: size

    integer(SI), parameter :: BIT_PER_BYTE = 8 ! assumption

    size = storage_size(i) / BIT_PER_BYTE
  end function sizeof_0d_di


  function sizeof_0d_si( i ) result(size)
    integer(SI), intent(in) :: i
    integer(SI) :: size

    integer(SI), parameter :: BIT_PER_BYTE = 8 ! assumption

    size = storage_size(i) / BIT_PER_BYTE
  end function sizeof_0d_si


  function sizeof_0d_dr( r ) result(size)
    real(DR), intent(in) :: r
    integer(SI) :: size

    integer(SI), parameter :: BIT_PER_BYTE = 8 ! assumption

    size = storage_size(r) / BIT_PER_BYTE
  end function sizeof_0d_dr


  function sizeof_0d_sr( r ) result(size)
    real(SR), intent(in) :: r
    integer(SI) :: size

    integer(SI), parameter :: BIT_PER_BYTE = 8 ! assumption

    size = storage_size(r) / BIT_PER_BYTE
  end function sizeof_0d_sr


  subroutine zeroset_dr01( d01 )
    real(DR), intent(out) :: d01
    d01 = 0.0_DR
  end subroutine zeroset_dr01


  subroutine zeroset_dr02( d01, d02 )
    real(DR), intent(out) :: d01, d02
    d01 = 0.0_DR
    d02 = 0.0_DR
  end subroutine zeroset_dr02


  subroutine zeroset_dr03( d01, d02, d03 )
    real(DR), intent(out) :: d01, d02, d03
    d01 = 0.0_DR
    d02 = 0.0_DR
    d03 = 0.0_DR
  end subroutine zeroset_dr03


  subroutine zeroset_dr04( d01, d02, d03, d04 )
    real(DR), intent(out) :: d01, d02, d03, d04
    d01 = 0.0_DR
    d02 = 0.0_DR
    d03 = 0.0_DR
    d04 = 0.0_DR
  end subroutine zeroset_dr04


  subroutine zeroset_dr_array01( d01 )
    real(DR), intent(out) :: d01(:,:,:)
    d01(:,:,:) = 0.0_DR
  end subroutine zeroset_dr_array01


  subroutine zeroset_dr_array02( d01, d02 )
    real(DR), intent(out) :: d01(:,:,:),  &
                      d02(:,:,:)
    d01(:,:,:) = 0.0_DR
    d02(:,:,:) = 0.0_DR
  end subroutine zeroset_dr_array02



  subroutine zeroset_dr_array03( d01, d02, d03 )
    real(DR), intent(out) :: d01(:,:,:),  &
                      d02(:,:,:),  &
                      d03(:,:,:)
    d01(:,:,:) = 0.0_DR
    d02(:,:,:) = 0.0_DR
    d03(:,:,:) = 0.0_DR
  end subroutine zeroset_dr_array03


  subroutine zeroset_dr_array04( d01, d02, d03, d04 )
    real(DR), intent(out) :: d01(:,:,:),  &
                      d02(:,:,:),  &
                      d03(:,:,:),  &
                      d04(:,:,:)
    d01(:,:,:) = 0.0_DR
    d02(:,:,:) = 0.0_DR
    d03(:,:,:) = 0.0_DR
    d04(:,:,:) = 0.0_DR
  end subroutine zeroset_dr_array04


  subroutine zeroset_sr01( f01 )
    real(SR), intent(out) :: f01
    f01 = 0.0_SR
  end subroutine zeroset_sr01


  subroutine zeroset_sr02( f01, f02 )
    real(SR), intent(out) :: f01, f02
    f01 = 0.0_SR
    f02 = 0.0_SR
  end subroutine zeroset_sr02


  subroutine zeroset_sr03( f01, f02, f03 )
    real(SR), intent(out) :: f01, f02, f03
    f01 = 0.0_SR
    f02 = 0.0_SR
    f03 = 0.0_SR
  end subroutine zeroset_sr03


  subroutine zeroset_sr04( f01, f02, f03, f04 )
    real(SR), intent(out) :: f01, f02, f03, f04
    f01 = 0.0_SR
    f02 = 0.0_SR
    f03 = 0.0_SR
    f04 = 0.0_SR
  end subroutine zeroset_sr04


  subroutine zeroset_sr_array01( f01 )
    real(SR), intent(out) :: f01(:,:,:)
    f01(:,:,:) = 0.0_SR
  end subroutine zeroset_sr_array01


  subroutine zeroset_sr_array02( f01, f02 )
    real(SR), intent(out) :: f01(:,:,:),  &
                      f02(:,:,:)
    f01(:,:,:) = 0.0_SR
    f02(:,:,:) = 0.0_SR
  end subroutine zeroset_sr_array02


  subroutine zeroset_sr_array03( f01, f02, f03 )
    real(SR), intent(out) :: f01(:,:,:),  &
                      f02(:,:,:),  &
                      f03(:,:,:)
    f01(:,:,:) = 0.0_SR
    f02(:,:,:) = 0.0_SR
    f03(:,:,:) = 0.0_SR
  end subroutine zeroset_sr_array03


  subroutine zeroset_sr_array04( f01, f02, f03, f04 )
    real(SR), intent(out) :: f01(:,:,:),  &
                      f02(:,:,:),  &
                      f03(:,:,:),  &
                      f04(:,:,:)
    f01(:,:,:) = 0.0_SR
    f02(:,:,:) = 0.0_SR
    f03(:,:,:) = 0.0_SR
    f04(:,:,:) = 0.0_SR
  end subroutine zeroset_sr_array04


!
! Private  これより上が非公開の関数・ルーチン
!===================================================
! Public   これより下が公開する関数・ルーチン
!


  function ut__appear_pos( tag, tag_list ) result(nth)
    integer(SI), intent(in) :: tag
    integer(SI), intent(in) :: tag_list(:)
    integer(SI) :: nth
      !!>
!        To find the integer nth that satisfies tag_list(nth) = tag.
!        Returns NIL if it could not find the member.
      !!<
    integer(SI) :: i

    do i = 1 , size(tag_list,dim=1)
       if ( tag_list(i)==tag ) then
          nth = i
          return
       end if
    end do
    nth = NIL
  end function ut__appear_pos


  subroutine ut__assert( must_be_true, message )
    !! アサーション
    logical    , intent(in) :: must_be_true  !! 必須条件
    character(len=*), intent(in) :: message       !! 遺言

    if ( .not. must_be_true ) then
       call ut__fatal( message )
    end if
  end subroutine ut__assert


  function ut__atan(y,x)
    real(DR), intent(in) :: y, x
    real(DR) :: ut__atan

      !!>
!        170628: Changed the input type from SR to DR.
!                 I do not know why it was SR. ---kage.
      !!<

    ut__atan = atan2(y,x)

      !!>
!        2004.04.05: redefined phi span: from -3pi/4 to 3pi/4.
!             if( ut__atan<0.0_DR ) ut__atan = ut__atan + TWOPI
      !!<
  end function ut__atan


  
  function ut__date_and_time() result(string)
    character(len=8)  :: date   ! e.g., "20220709"
    character(len=10) :: time   ! "HHMMSS.sss"
    character(len=4)  :: year
    character(len=2)  :: month, day, hour, minute, second
    character(len=10) :: year_month_day     ! e.g., "2022.07.09"
    character(len=8)  :: hour_minute_second ! e.g., "16:56:00"
    character(len=19) :: string ! e.g., "2022.07.09/16:56:00"

    call date_and_time(date, time)

    year   = date(1:4)
    month  = date(5:6)
    day    = date(7:8)
    hour   = time(1:2)
    minute = time(3:4)
    second = time(5:6)

    year_month_day     = year // '.' // month // '.' // day
    hour_minute_second = hour // ':' // minute // ':' // second

    string = year_month_day // '/' // hour_minute_second

  end function ut__date_and_time


  subroutine ut__fatal( last_will )
    character(len=*), intent(in) :: last_will
    !! 致命的エラー
    !!
    !!   遺言を書き出してプログラムを停止する
    !!
    call ut__deco_message( '!', last_will )
    print *, 'Program stopped by ut__fatal.'
    stop
  end subroutine ut__fatal


  function ut__gets( file_unit, end_of_file )
    integer(SI), intent(in) :: file_unit
    logical   , intent(out) :: end_of_file
    character(len=UT__STRLEN_MAX) :: ut__gets

    !!>
!      f90 version of gets (get-string). To read a line of text or
!      string from the input file specified by the unit number of file_unit.
!
!      Developed by Akira kageyama on 2006.02.01.
!
!      Usage:
!         e.g., print *, trim(ut__gets(FILE_STANDARD_IN))
    !!<

    character(len=7) :: form   ! when UT__STRLEN_MAX=200, form = "(a0200)"
                                              ! len=7 comes from "1234567".
    end_of_file = .false.
    if ( UT__STRLEN_MAX >= 10000 ) then
       ! You should do; (i) make len=8 of form; (ii) (a2,i5.5,a1), below.
       call ut__fatal("<ut__text_line> UT__STRLEN_MAX too large."        &
                    //" Increase len (=7, now) of form.")
    end if
    write(form,'(a2,i4.4,a1)') "(a",UT__STRLEN_MAX,")"
    read(file_unit,form,end=1000) ut__gets
    return
    1000 end_of_file = .true.
  end function ut__gets


  function ut__i2c1( i )
    integer(SI), intent(in) :: i
    character(len=1)      :: ut__i2c1

    ! Convert an integer into a character.

    if ( i < 0 ) then
       ut__i2c1 = 'N'
    else if ( i > 9 ) then
       ut__i2c1 = 'X'
    else
       write(ut__i2c1,'(i1.1)') i
    end if
  end function ut__i2c1


  function ut__i2c3( i )
    integer(SI), intent(in) :: i
    character(len=3)      :: ut__i2c3

    ! Convert an integer into 3 characters.
    !             e.g., i=10 --> str3="010"

    if ( i < 0 ) then
       ut__i2c3 = 'NNN'
    else if ( i > 999 ) then
       ut__i2c3 = 'XXX'
    else
       write(ut__i2c3,'(i3.3)') i
    end if
  end function ut__i2c3


  function ut__i2c4( i )
    integer(SI), intent(in) :: i
    character(len=4)      :: ut__i2c4

    ! Convert an integer into 4 characters.
    !             e.g., i=10 --> str4="0010"

    if ( i < 0 ) then
       ut__i2c4 = 'NNNN'
    else if ( i > 9999 ) then
       ut__i2c4 = 'XXXX'
    else
       write(ut__i2c4,'(i4.4)') i
    end if
  end function ut__i2c4


  function ut__i2c7( i )
    integer(SI), intent(in) :: i
    character(len=7) :: ut__i2c7

    ! Convert an integer into 7 characters.
    !             e.g., i=10 --> str7="0000010"

    if ( i < 0 ) then
       ut__i2c7 = 'NNNNNNN'
    else if ( i > 9999999 ) then
       ut__i2c7 = 'XXXXXXX'
    else
       write(ut__i2c7,'(i7.7)') i
    end if
  end function ut__i2c7


  subroutine ut__make_queue( tag,              &
                             tag_list,         &
                             tag_counts,       &
                             ntags,            &
                             nth )
    integer(SI), intent(in)  :: tag
    integer(SI), intent(inout)  :: tag_list(:)
    integer(SI), intent(inout)  :: tag_counts(:)
    integer(SI), intent(out) :: ntags
    integer(SI), intent(out) :: nth

    !!>
!
!      Basic relation:  tag_list(nth) = tag
!
!       The initial condition of tag_list and tag_counts are
!       supposed to be:
!
!          tag_list(1) = NIL,     tag_counts(1) = 0
!          tag_list(2) = NIL,     tag_counts(2) = 0
!          tag_list(3) = NIL,     tag_counts(3) = 0
!              .                       .
!              .                       .
!              .                       .
!
!       When you call as follows
!          call ut__make_queue(30,tag_list,tag_counts,nth)  !=>  nth = 1
!          call ut__make_queue(21,tag_list,tag_counts,nth)  !=>  nth = 2
!          call ut__make_queue( 5,tag_list,tag_counts,nth)  !=>  nth = 3
!          call ut__make_queue(12,tag_list,tag_counts,nth)  !=>  nth = 4
!          call ut__make_queue(30,tag_list,tag_counts,nth)  !=>  nth = 1
!          call ut__make_queue( 5,tag_list,tag_counts,nth)  !=>  nth = 3
!          call ut__make_queue( 5,tag_list,tag_counts,nth)  !=>  nth = 3
!          call ut__make_queue(18,tag_list,tag_counts,nth)  !=>  nth = 5
!
!                        ||            c o n t e t s
!             l i s t    ||----------------------------------------
!                        ||     1     |     2     |     3     | ...
!         ---------------++----------------------------------------
!                1       ||    30     |    30     |           |
!         ---------------------------------------------------------
!                2       ||    21     |           |           |
!         ---------------------------------------------------------
!                3       ||     5     |     5     |     5     |
!         ---------------------------------------------------------
!                4       ||    12     |           |           |
!         ---------------------------------------------------------
!                5       ||    18     |           |           |
!         ---------------------------------------------------------
!                :       ||           |           |           |
!
!         tag_list must be equal tag_contents
!
!
!       The resulting two arrays tag_list(:) and tag_counts(:) are
!
!          tag_list(1) = 30         tag_counts(1) = 2
!          tag_list(2) = 21         tag_counts(2) = 1
!          tag_list(3) = 5          tag_counts(3) = 3
!          tag_list(4) = 12         tag_counts(4) = 1
!          tag_list(5) = 18         tag_counts(5) = 1
!          tag_list(6) = NIL        tag_counts(6) = 0
!          tag_list(7) = NIL        tag_counts(7) = 0
!          tag_list(8) = NIL        tag_counts(8) = 0
!
!       and ntags = 5.
!
    !!<

    integer(SI) :: i

    call ut__assert( size(tag_list,dim=1)==size(tag_counts,dim=1),  &
                     '<ut__make_queue> size inconsistent.')

    nth = 0
    ntags = 0

    ! Count how many tags already registered.
    do i = 1 , size(tag_list,dim=1)
       if ( tag_list(i)==NIL ) exit
       ntags = i
    end do

    ! If the input "tag" is previously registered,
    ! the following do loop works.
    do i = 1 , ntags
       if ( tag_list(i)==tag ) then
          nth = i
          tag_counts(i) = tag_counts(i) + 1
          return
       end if
    end do

    ! This "tag" is a new one.
    ntags = ntags + 1
    if (ntags > size(tag_list,dim=1)) &
         call ut__fatal('<ut__make_queue> Too many tags.')
                        ! The remedy is simple. Enlarge tag_list size.

    nth = ntags
    tag_list(ntags) = tag
    tag_counts(ntags) = tag_counts(ntags) + 1
  end subroutine ut__make_queue


  subroutine ut__sleep( second )
    real(SR), intent(in) :: second
    !!>
!      Sleeps for roughly "second" seconds. Not very exact.
!         Sample:
!         ----
!         !  program test
!         !  use const_m
!         !  use ut_m
!         !  implicit none
!         !    character(len=10) :: time
!         !    call date_and_time(time=time)
!         !    print *, time
!         !    call ut__sleep(1.2)
!         !    call date_and_time(time=time)
!         !    print *, time
!         !  end program
!         ----
    !!<

    character(len=10) :: time_stt, time_now
    real(DR) :: double_time_stt, double_time_now
    real(DR) :: dt

    call date_and_time(time=time_stt)
    read(time_stt,'(f12.3)') double_time_stt
    do
       call date_and_time(time=time_now)
       call iTime_killer
       read(time_now,'(f12.3)') double_time_now
       if ( double_time_now < double_time_stt ) then ! passed midnight.
         double_time_now = double_time_now + 240000.000_DR
       end if
       dt = double_time_now - double_time_stt
       if ( real(dt,SR) >= second ) exit
    end do

  contains

    subroutine iTime_killer
      integer :: i
      real(DR) :: x
      x = 1.0_DR
      do i = 1 , 10**4
        x = sin(x)
      end do
    end subroutine

  end subroutine


  function ut__smooth_step( x, width )
    real(DR), intent(in) :: x
    real(DR), intent(in) :: width
    real(DR) :: ut__smooth_step

    !!>
!      A "smooth" step function
!
!                          |
!                       1.0|      +++++++++
!                          |   +
!                          | +
!                          |+
!                          +
!                         +|
!                        + |
!                      +   |
!           +++++++++------+-0.0--------------> x
!                    |     |     |
!                    |<--width-->|
!                          |
    !!<

    real(DR) :: width_half_inv

    width_half_inv = 2.0_DR / width  ! = 1/(width/2)

    ut__smooth_step = 0.5_DR + 0.5_DR * tanh(x*width_half_inv)
  end function ut__smooth_step


  function ut__step( x )
    real(DR), intent(in) :: x
    real(DR) :: ut__step

    !!>
!      Step function with special care for the case x=0.
!
!                      ***********  1.0
!                      *
!                      X 0.5
!                      *
!      0.0  ***********|-------------> x
!                      0
    !!<

    if (x > 0.0_DR)       then
       ut__step = 1.0_DR
    else if (x < 0.0_DR) then
       ut__step = 0.0_DR
    else      !  x is exactly 0.0_DR
       ut__step = 0.5_DR
    end if
  end function ut__step

end module ut_m
