module mod_arrays
  implicit none

  private
  public :: reverse, average, std
  
contains

  pure function reverse(a)
    real, intent(in) :: a(:)
    real :: reverse(size(a))
    reverse = a(size(a):1:-1)
  end function reverse
  
  pure real function average(x)
    real, intent(in) :: x(:)
    average = sum(x) / size(x)
  end function average

  pure real function std(x)
    real, intent(in) :: x(:)
    std = sqrt(average((x - average(x))**2))
  end function std

end module mod_arrays