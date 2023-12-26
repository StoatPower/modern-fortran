module mod_diff

   use iso_fortran_env, only: int32, real32
   implicit none

contains

   ! applies the periodic boundary condition on the left (element on left edge of domain)
   ! because we're applying periodic (cyclic) boundary conditions and then
   ! computes the finite difference of input array
   pure function diff(x) result(dx)
      real(real32), intent(in) :: x(:)  ! assumed-shape real array as input argument
      real(real32) :: dx(size(x))       ! the result will be a real array of the same size as x
      integer(int32) :: im
      im = size(x)
      dx(1) = x(1) - x(im)      ! calculates the boundary value
      dx(2:im) = x(2:im) - x(1:im-1)  ! calculates the finite diff for all other elements of x
   end function diff

end module mod_diff
