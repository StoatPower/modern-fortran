module mod_io

  use mod_alloc, only: alloc

  implicit none

  private
  public :: num_records

contains

  integer function num_records(filename)
    ! Return the # of records (lines) of a text file
    character(len=*), intent(in) :: filename
    integer :: fileunit
    
    open(newunit=fileunit, file=filename)
    
    num_records = 0

    do
      read(unit=fileunit, fmt=*, end=1)
      num_records = num_records + 1
    end do
    1 continue
    close(unit=fileunit)
  end function num_records

end module mod_io