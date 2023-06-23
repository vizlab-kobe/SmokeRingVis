module vis_m
  use constants_m
  use field_m
  use ut_m
  use solver_m
  implicit none
  private
  public :: vis

  type, public :: vis_t
  contains
    procedure, nopass :: initialize => vis__initialize
    procedure, nopass :: finalize => vis__finalize
    procedure, nopass :: visualize => vis__visualize
    procedure, nopass :: get_enstrophy => vis__get_enstrophy
  end type vis_t

  type(vis_t) :: vis

contains

  subroutine vis__initialize(job_count)
    integer ::job_count

  end subroutine vis__initialize


  subroutine vis__visualize( step, fluid, visualization_count)
    type(field__fluid_t) ,intent(in) :: fluid
    integer(DI) ::step               !シミュレーションのステップ数
    integer ::step_integer       
    integer :: visualization_count   !可視化ステップ数

    type(field__vector3d_t) :: vel !! 速度場（3D）
    type(field__vector3d_t)       :: vor       ! vorticity、渦度
    real(DR), dimension(NX,NY,NZ) :: enstrophy ! 渦度の2乗
!    real(DR) :: enstrophy_max, enstrophy_min


    step_integer = int(step)
    call solver__get_subfield(fluid,vel)
    vor = operator_curl(vel)
    enstrophy = operator_dot_product(vor,vor)
      
  end subroutine vis__visualize

  function vis__get_enstrophy( fluid )
    type(field__fluid_t) ,intent(in) :: fluid
    type(field__vector3d_t) :: vel !! 速度場（3D）
    type(field__vector3d_t)       :: vor       ! vorticity、渦度
    real(DR), dimension(NX,NY,NZ) :: vis__get_enstrophy ! 渦度の2乗
    call solver__get_subfield(fluid,vel)
    vor = operator_curl(vel)
    vis__get_enstrophy = operator_dot_product(vor,vor)
  end function vis__get_enstrophy

  subroutine vis__finalize

  end subroutine vis__finalize

end module vis_m

