!-------------------------------------------------------------------
! class-hpc-smoke-ring: A simple sample field solver.
!
!    by Akira Kageyama, Kobe University, Japan.
!       email: sgks@mac.com
!
!    This software is released under the MIT License.
!
!-------------------------------------------------------------------
!    src/debug.f90
!-------------------------------------------------------------------

module debug_m
  use constants_m
  use ut_m
  use params_m
  implicit none

  private
  public :: debug__print

  interface debug__print
    module procedure print_str,             &
                     print_str_dint,        &
                     print_str_dint_double, &
                     print_str_double,      &
                     print_str_sint,        &
                     print_str_sint_double
  end interface


contains


  subroutine print_str(string)
    character(len=*), intent(in) :: string

    if (params__get_logical('Debug')) then
      call ut__message('debug: '//string)
    end if
  end subroutine print_str


  subroutine print_str_double(string, double)
    character(len=*), intent(in) :: string
    real(DR), intent(in)         :: double

    if (params__get_logical('Debug')) then
      call ut__message('debug: '//string, double)
    end if
  end subroutine print_str_double


  subroutine print_str_dint(string, int)
    character(len=*), intent(in) :: string
    integer(DI), intent(in)      :: int

    if (params__get_logical('Debug')) then
      call ut__message('debug: '//trim(string), int)
    end if
  end subroutine print_str_dint


  subroutine print_str_sint(string, int)
    character(len=*), intent(in) :: string
    integer(SI), intent(in)      :: int

    if (params__get_logical('Debug')) then
      call ut__message('debug: '//trim(string), int)
    end if
  end subroutine print_str_sint


  subroutine print_str_dint_double(string, i1, d1)
    character(len=*), intent(in) :: string
    integer(DI), intent(in)      :: i1
    real(DR), intent(in)         :: d1

    if (params__get_logical('Debug')) then
      call ut__message('debug: '//trim(string), i1, d1)
    end if
  end subroutine print_str_dint_double


  subroutine print_str_sint_double(string, i1, d1)
    character(len=*), intent(in) :: string
    integer(SI), intent(in)      :: i1
    real(DR), intent(in)         :: d1

    if (params__get_logical('Debug')) then
      call ut__message('debug: '//trim(string), i1, d1)
    end if
  end subroutine print_str_sint_double

end module debug_m
