module mod_gaussian

   use iso_fortran_env, only: int32, real32
   implicit none

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

end module mod_gaussian
