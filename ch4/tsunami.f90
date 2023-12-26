program tsunami

  use iso_fortran_env, only: int32, real32
  use mod_diff, only: diff

  ! for more functional goodness see https://wavebitscientific.github.io/functional-fortran/

  implicit none

  integer(int32) :: n                                ! loop iterations (n)

  integer(int32), parameter :: grid_size = 100       ! array size
  integer(int32), parameter :: num_time_steps = 100  ! num iterations
  real(real32), parameter :: dt = 1.                  ! time step [s]
  real(real32), parameter :: dx = 1.                  ! grid spacing [m]
  real(real32), parameter :: c = 1.                   ! phase speed [m/s]

  ! declares h as a real array with the number of elements equal to grid_size
  ! short-hand for `real, dimension(grid_size) :: h`
  real(real32) :: h(grid_size)  ! water height
  
  ! central index and decay factor of the water height Gaussian shape
  integer(int32), parameter :: icenter = 25
  real(real32), parameter :: decay = 0.02

  if (grid_size <= 0) stop 'grid_size must be > 0'
  if (dt <= 0) stop 'time step dt must be > 0'
  if (dx <= 0) stop 'grid spacing dx must be > 0'
  if (c <= 0) stop 'background flow speed c must be > 0'
  
  ! initialize the water height
  call set_gaussian(h, icenter, decay)

  print *, 0, h

  ! iterating the solution forward in time
  time_loop: do n = 1, num_time_steps
    ! we can substitute this one-liner in place of our looping
    ! because h and diff(h) are of the same shape (1d) and size
    ! computes the finite difference of water height on the fly
    ! by calling the diff function
    h = h - c * diff(h) / dx * dt
    print *, n, h
  end do time_loop

contains

  ! initializing the water height with a Gaussian shape
  ! icenter and decay control the position and width of the water height perturbation, respectively
  ! take note, can we do the following assignment in parallel? using `do concurrent`?
  ! TIP: look for whether any iteration depends on data calculated in any other iteration
  ! here, the right side depends only on teh loop counter i and params decay and icenter,
  ! wheras the variable on the left side (h(i)) is not used on the right side
  pure subroutine set_gaussian(x, icenter, decay)
    real(real32), intent(in out) :: x(:) ! 1d array as input/output argument
    integer(int32), intent(in) :: icenter
    real(real32), intent(in) :: decay
    integer(int32) :: i
    do concurrent(i = 1:size(x))
      x(i) = exp(-decay * (i - icenter)**2)
    end do
  end subroutine set_gaussian

end program tsunami