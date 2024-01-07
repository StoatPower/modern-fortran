module mod_arrays
  implicit none

  private
  public :: reverse, average, std, moving_average, &
            moving_std, crossneg, crosspos
  
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

  pure function crosspos(x, w) result(res)
    ! Returns the crossover from low to high value 
    real, intent(in) :: x(:)
    integer, intent(in) :: w
    ! we don't know the size ahead of time, so it will be dynamic
    integer, allocatable :: res(:)
    ! array to store the moving average of x
    real, allocatable :: xavg(:)
    ! logical (boolean) arrays to mask x
    logical, allocatable :: greater(:), lesser(:)
    integer :: i
    ! first guess result, all indices but the first
    res = [(i, i = 2, size(x))]
    ! computes the moving average
    xavg = moving_average(x, w)
    ! logical arrays to tell us where x is greater or smaller
    greater = x > xavg
    lesser = x < xavg
    ! uses built-in function pack to subset an array according to a cond
    ! uses automatic reallocation on assignment from sec 5.2.5
    res = pack(res, greater(2:) .and. lesser(:size(x) - 1))    
  end function crosspos

  pure function crossneg(x, w) result(res)
    ! Returns the crossover from high to low value 
    real, intent(in) :: x(:)
    integer, intent(in) :: w
    ! we don't know the size ahead of time, so it will be dynamic
    integer, allocatable :: res(:)
    ! array to store the moving average of x
    real, allocatable :: xavg(:)
    ! logical (boolean) arrays to mask x
    logical, allocatable :: greater(:), lesser(:)
    integer :: i
    ! first guess result, all indices but the first
    res = [(i, i = 2, size(x))]
    ! computes the moving average
    xavg = moving_average(x, w)
    ! logical arrays to tell us where x is greater or smaller
    greater = x > xavg
    lesser = x < xavg
    ! uses built-in function pack to subset an array according to a cond
    ! uses automatic reallocation on assignment from sec 5.2.5
    res = pack(res, lesser(2:) .and. greater(:size(x) - 1))    
  end function crossneg

end module mod_arrays