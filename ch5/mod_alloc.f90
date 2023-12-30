program mod_alloc

   implicit none

contains

   subroutine alloc(a, im)
      real, allocatable, intent(in out) :: a(:)
      integer, intent(in) :: im
      integer :: stat
      character(:), allocatable :: errmsg

      if (.not.allocated(a)) then
         allocate(a(im), stat=stat, errmsg=errmsg)
         if (stat /= 0) then
            print *, errmsg
         end if
      end if

   end subroutine alloc

   subroutine free(a)
      real, allocatable, intent(in out) :: a(:)
      integer :: stat
      character(:), allocatable :: errmsg

      if (allocated(a)) then
         deallocate(a, stat=stat, errmsg=errmsg)
         if (stat /= 0) then
            print *, errmsg
         end if
      end if
   end subroutine free

end program mod_alloc
