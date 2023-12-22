program tsunami

  implicit none

  integer :: i, n                             ! array elements (i) and loop iterations (n)
  integer, parameter :: grid_size = 100       ! array size
  integer, parameter :: num_time_steps = 100  ! num iterations

  real, parameter :: dt = 1.                  ! time step [s]
  real, parameter :: dx = 1.                  ! grid spacing [m]
  real, parameter :: c = 1.                   ! phase speed [m/s]

  ! declares h as a real array with the number of elements equal to grid_size
  ! short-hand for `real, dimension(grid_size) :: h`
  real :: h(grid_size)  ! water height
  real :: dh(grid_size) ! finite difference in water height
  
  ! central index and decay factor of the water height Gaussian shape
  integer, parameter :: icenter = 25
  real, parameter :: decay = 0.02

  if (grid_size <= 0) stop 'grid_size must be > 0'
  if (dt <= 0) stop 'time step dt must be > 0'
  if (dx <= 0) stop 'grid spacing dx must be > 0'
  if (c <= 0) stop 'background flow speed c must be > 0'
  
  ! initializing the water height with a Gaussian shape
  ! icenter and decay control the position and width of the water height perturbation, respectively
  ! take note, can we do the following assignment in parallel? using `do concurrent`?
  ! TIP: look for whether any iteration depends on data calculated in any other iteration
  ! here, the right side depends only on teh loop counter i and params decay and icenter,
  ! wheras the variable on the left side (h(i)) is not used on the right side
  
  ! non parallel version
  ! do i = 1, grid_size
  !   h(i) = exp(-decay * (i - icenter)**2) ! ** is power operator
  ! end do

  ! parallel version
  do concurrent (i = 1:grid_size) 
    h(i) = exp(-decay * (i - icenter)**2)
  end do

  ! iterating the solution forward in time
  time_loop: do n = 1, num_time_steps

    dh = diff(h) ! calculates the difference in a fn

    do concurrent (i = 1:grid_size)
      h(i) = h(i) - c * dh(i) / dx * dt ! evaluates h at the next time step
    end do

    print *, n, h
  end do time_loop

contains

  ! applies the periodic boundary condition on the left (element on left edge of domain)
  ! because we're applying periodic (cyclic) boundary conditions, dh(1) depends on the value
  ! of h from the right edge of the domain
  function diff(x) result(dx)
    real, intent(in) :: x(:)  ! assumed-shape real array as input argument
    real :: dx(size(x))       ! the result will be a real array of the same size as x
    integer :: im
    im = size(x)
    dx(1) = x(1) - x(im)      ! calculates the boundary value
    dx(2:im) = x(2:im) - x(1:im-1)  ! calculates the finite diff for all other elements of x
  end function diff

end program tsunami