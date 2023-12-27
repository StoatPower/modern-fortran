module mod_diff

   use iso_fortran_env, only: int32, real32
   implicit none

contains

   ! applies the periodic boundary condition on the left (element on left edge of domain)
   ! because we're applying periodic (cyclic) boundary conditions and then
   ! computes the finite difference of input array
   ! This is the UPWIND finite difference, meaning it is the difference in
   ! ONE direction, as it is the diff between the current element and the
   ! previous element
   pure function diff_upwind(x) result(dx)
      real(real32), intent(in) :: x(:)  ! assumed-shape real array as input argument
      real(real32) :: dx(size(x))       ! the result will be a real array of the same size as x
      integer(int32) :: im
      im = size(x)
      dx(1) = x(1) - x(im)            ! calculates the boundary value
      dx(2:im) = x(2:im) - x(1:im-1)  ! calculates the finite diff for all other elements of x
   end function diff_upwind

   pure function diff_centered(x) result(dx)
      real(real32), intent(in) :: x(:)
      real(real32) :: dx(size(x))
      integer(int32) :: im
      im = size(x)
      dx(1) = x(2) - x(im)             ! calculates the boundary value on the left
      dx(im) = x(1) - x(im-1)          ! calculates the boundary value on the right
      dx(2:im-1) = x(3:im) - x(1:im-2) ! calculates the difference in the interior
      ! divides all elements by 2
      ! in theory, it is preferred to multiply by 0.5
      ! bc for fp numbers, mult is faster than div.
      dx = 0.5 * dx                    
   end function diff_centered

end module mod_diff
