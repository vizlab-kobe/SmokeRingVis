
module rank_m
  use constants_m
  implicit none

  private
  public :: rank__initialize, &
            rank__peripheral

  type, public ::  rank__div_xyz_t
    integer(SI) :: rank_x, rank_y, rank_z
  end type rank__div_xyz_t
  
  type rank_peripheral_t
    integer(SI) :: lower, upper
  end type rank_peripheral_t

  type, public :: rank__peripheral_xyz_t
    type(rank_peripheral_t) :: x, y, z
  end type rank__peripheral_xyz_t


contains

  !need customize depending on nprocs
  subroutine rank__initialize(myrank, rank_div)
    implicit none
    type(rank__div_xyz_t), intent(inout) :: rank_div
    integer(SI), intent(in) :: myrank
    
    ! X_NPROCS = 3, Y_NPROCS = 2, Z_NPROCS = 2 
    !
    ! myrank rank_x rank_y rank_z
    !   0       0     0      0
    !   1       0     0      1
    !   2       0     1      0
    !   3       0     1      1
    !              :
    !              :   
    !   8       2     0      0
    !              :
    !              : 
    !   11      2     1      1

    rank_div%rank_x = myrank / 4
   
    if(mod(myrank, 4) == 0 .or. mod(myrank,4) == 1) then
      rank_div%rank_y = 0
    else
      rank_div%rank_y = 1
    end if

    rank_div%rank_z = mod(myrank,2)
    
    print *, "rank_xyz", myrank, rank_div%rank_x, rank_div%rank_y, rank_div%rank_z
 
  end subroutine rank__initialize
  
  !need customize depending on nprocs
  function rank__reverse(rank_x, rank_y, rank_z)
    integer(SI), intent(in) :: rank_x, rank_y, rank_z
    integer(SI) :: rank__reverse

    rank__reverse = rank_x * 4 + rank_y * 2 + rank_z

  end function rank__reverse


  subroutine rank__peripheral(rank_div, peripheral)
    implicit none
    type(rank__div_xyz_t), intent(in) :: rank_div
    type(rank__peripheral_xyz_t), intent(inout) :: peripheral
    integer(SI) :: lower_x, lower_y, lower_z
    integer(SI) :: upper_x, upper_y, upper_z

    !<<x>>
    if(rank_div%rank_x == 0) then
      lower_x = X_NPROCS - 1
    else
      lower_x = rank_div%rank_x - 1
    end if

    peripheral%x%lower = rank__reverse(lower_x, rank_div%rank_y, rank_div%rank_z)

    if(rank_div%rank_x == X_NPROCS -1) then
      upper_x = 0
    else
      upper_x = rank_div%rank_x + 1
    end if

    peripheral%x%upper = rank__reverse(upper_x, rank_div%rank_y, rank_div%rank_z)

    !<<y>>
    if(rank_div%rank_y == 0) then
      lower_y = Y_NPROCS - 1
    else
      lower_y = rank_div%rank_y - 1
    end if

    peripheral%y%lower = rank__reverse(rank_div%rank_x, lower_y, rank_div%rank_z)

    if(rank_div%rank_y == Y_NPROCS - 1) then
      upper_y = 0
    else
      upper_y = rank_div%rank_y + 1
    end if

    peripheral%y%upper = rank__reverse(rank_div%rank_x, upper_y, rank_div%rank_z)

    !<<z>>
    if(rank_div%rank_z == 0) then
      lower_z = Z_NPROCS - 1
    else
      lower_z = rank_div%rank_z - 1
    end if

    peripheral%z%lower = rank__reverse(rank_div%rank_x, rank_div%rank_y, lower_z)

    if(rank_div%rank_z == Z_NPROCS - 1) then
      upper_z = 0
    else
      upper_z = rank_div%rank_z + 1
    end if
   

    peripheral%z%upper = rank__reverse(rank_div%rank_x, rank_div%rank_y, upper_z)

  end subroutine rank__peripheral

end module rank_m
 
