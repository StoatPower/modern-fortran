program tsunami

  use iso_fortran_env, only: int32, real32
  use mod_diff, only: diff => diff_centered
  use mod_gaussian, only: set_gaussian

  ! for more functional goodness see https://wavebitscientific.github.io/functional-fortran/

  implicit none

  integer(int32) :: n                                ! loop iterations (n)

  integer(int32), parameter :: grid_size = 100       ! array size
  integer(int32), parameter :: num_time_steps = 5000  ! num iterations
  real(real32), parameter :: dt = 0.02                ! time step [s]
  real(real32), parameter :: dx = 1.                  ! grid spacing [m]
  real(real32), parameter :: g = 9.8                  ! gravitational acc
  real(real32), parameter :: hmean = 10

  ! declares h as a real array with the number of elements equal to grid_size
  ! short-hand for `real, dimension(grid_size) :: h`
  real(real32) :: h(grid_size)  ! water height
  real(real32) :: u(grid_size)  ! water velocity
  
  ! central index and decay factor of the water height Gaussian shape
  integer(int32), parameter :: icenter = 25
  real(real32), parameter :: decay = 0.02

  if (grid_size <= 0) stop 'grid_size must be > 0'
  if (dt <= 0) stop 'time step dt must be > 0'
  if (dx <= 0) stop 'grid spacing dx must be > 0'
  
  ! initialize the water height and velocity
  call set_gaussian(h, icenter, decay)
  u = 0

  print *, 0, h

  ! iterating the solution forward in time
  time_loop: do n = 1, num_time_steps
    u = u - (u * diff(u) + g * diff(h)) / dx * dt ! calc water velocity
    h = h - diff(u * (hmean + h)) / dx * dt       ! calc watr height
    print *, n, h
  end do time_loop

end program tsunami