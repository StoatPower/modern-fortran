module mod_arrays
  implicit none

  private
  public :: reverse, average, std, moving_average, moving_std
  
contains

  pure function reverse(a)
    ! Reverses the order of elements of a.
    real, intent(in) :: a(:)
    real :: reverse(size(a))
    reverse = a(size(a):1:-1)
  end function reverse
  
  pure real function average(x)
    ! Returns the average of x.
    real, intent(in) :: x(:)
    average = sum(x) / size(x)
  end function average

  pure real function std(x)
    ! Returns the standard deviation of x.
    real, intent(in) :: x(:)
    std = sqrt(average((x - average(x))**2))
  end function std

  pure function moving_average(x, w) result(res)
    ! Returns the moving average of x with one-sided window w.
    real, intent(in) :: x(:)
    integer, intent(in) :: w
    real :: res(size(x))
    integer :: i, i1
    do i = 1, size(x)
      i1 = max(i-w, 1)
      res(i) = average(x(i1:i))
    end do
  end function moving_average

  pure function moving_std(x, w) result(res)
    ! Returns the moving standard deviation of x with one-sided window w
    real, intent(in) :: x(:)
    integer, intent(in) :: w
    real :: res(size(x))
    integer :: i, i1
    do i = 1, size(x)
      i1 = max(i-w, 1)
      res(i) = std(x(i1:i))
    end do
  end function moving_std

end module mod_arrays