module InSituVis_m
  use iso_c_binding
  implicit none

  private

  ! C interface
  interface
     function C_InSituVis_new()&
          bind( C, name="InSituVis_new" )
       import
       type( C_ptr ) :: C_InSituVis_new
     end function C_InSituVis_new

     subroutine C_InSituVis_delete( this )&
          bind( C, name="InSituVis_delete" )
       import
       type( C_ptr ), value :: this
     end subroutine C_InSituVis_delete

     subroutine C_InSituVis_initialize( this )&
          bind( C, name="InSituVis_initialize" )
       import
       type( C_ptr ), value :: this
     end subroutine C_InSituVis_initialize

     subroutine C_InSituVis_finalize( this )&
          bind( C, name="InSituVis_finalize" )
       import
       type( C_ptr ), value :: this
     end subroutine C_InSituVis_finalize

     subroutine C_InSituVis_put( this, values, nvalues, dimx, dimy, dimz )&
          bind( C, name="InSituVis_put" )
       import
       type( C_ptr ),    value :: this
       real( C_float )         :: values( nvalues )
       integer( C_int ), value :: nvalues
       integer( C_int ), value :: dimx
       integer( C_int ), value :: dimy
       integer( C_int ), value :: dimz
     end subroutine C_InSituVis_put

     subroutine C_InSituVis_exec( this, time_value, time_index )&
          bind( C, name="InSituVis_exec" )
       import
       type( C_ptr ),    value :: this
       real( C_float ),  value :: time_value
       integer( C_int ), value :: time_index
     end subroutine C_InSituVis_exec
  end interface

  ! Class definition
  public :: InSituVis
  type InSituVis
     private
     type( C_ptr ) :: ptr = C_NULL_ptr
   contains
     final :: InSituVis_destroy ! Destructor
     procedure :: delete => InSituVis_delete
     procedure :: initialize => InSituVis_initialize
     procedure :: finalize => InSituVis_finalize
     procedure :: put => InSituVis_put
     procedure :: exec => InSituVis_exec
  end type InSituVis

  interface InSituVis ! Constructor (Instancer)
     procedure InSituVis_new
  end interface InSituVis

contains

  function InSituVis_new()
    implicit none
    type( InSituVis ) :: InSituVis_new
    InSituVis_new % ptr = C_InSituVis_new()
  end function InSituVis_new

  subroutine InSituVis_destroy( this )
    implicit none
    type( InSituVis ) :: this
    if ( c_associated( this % ptr ) ) then
       call C_InSituVis_delete( this % ptr )
       this % ptr = C_NULL_ptr
    endif
  end subroutine InSituVis_destroy

  subroutine InSituVis_delete( this )
    implicit none
    class( InSituVis ) :: this
    call C_InSituVis_delete( this % ptr )
    this % ptr = C_NULL_ptr
  end subroutine InSituVis_delete

  subroutine InSituVis_initialize( this )
    implicit none
    class( InSituVis ), intent( in ) :: this
    call C_InSituVis_initialize( this % ptr )
  end subroutine InSituVis_initialize

  subroutine InSituVis_finalize( this )
    implicit none
    class( InSituVis ), intent( in ) :: this
    call C_InSituVis_finalize( this % ptr )
  end subroutine InSituVis_finalize

  subroutine InSituVis_put( this, values, nvalues, dimx, dimy, dimz )
    implicit none
    class( InSituVis ), intent( in ) :: this
    real( C_float ),    intent( in ) :: values(nvalues)
    integer( C_int ),   intent( in ) :: nvalues
    integer( C_int ),   intent( in ) :: dimx
    integer( C_int ),   intent( in ) :: dimy
    integer( C_int ),   intent( in ) :: dimz
    call C_InSituVis_put( this % ptr, values, nvalues, dimx, dimy, dimz )
  end subroutine InSituVis_put

  subroutine InSituVis_exec( this, time_value, time_index )
    implicit none
    class( InSituVis ), intent( in ) :: this
    real( C_float ),    intent( in ) :: time_value
    integer( C_int ),   intent( in ) :: time_index
    call C_InSituVis_exec( this % ptr, time_value, time_index )
  end subroutine InSituVis_exec

end module InSituVis_m
