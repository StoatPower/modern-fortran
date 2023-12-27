module mod_circle

  implicit none
  private :: pi ! defines pi as private parameter
  real, parameter :: pi = 3.14159256
  ! can just use `private` to declare all variables private
  ! and then explicitly declare any public variables with `public :: `

contains

  real pure elemental function circle_area(r) result(a)
    real, intent(in) :: r
    a = r**2 * pi
  end function circle_area

end module mod_circle