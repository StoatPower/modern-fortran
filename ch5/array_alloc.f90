program array_alloc

  implicit none

  ! examples of different ways to allocate an array
  ! here we see the so-called *implied do loop constructor*

  integer, allocatable :: a(:)
  integer :: i

  real, allocatable :: b(:)
  integer :: j
  real, parameter :: pi = 3.14159256

  ! elements will range from 1 to 100
  a = [(i, i = 1, 100)] 

  ! initializes an array with sines from 0 to 2pi in 1000 steps
  b = [(sin(2 * pi * j / 1000.), j = 0, 1000)]
  
  ! Finally, Fortran also lets you create empty arrays using [integer ::] or [real ::]. In
  ! practice, these could be useful if invoking a generator—a function that appends an element
  ! to an array on every call.

end program array_alloc