module mod_io

  use mod_alloc, only: alloc

  implicit none

  private
  public :: read_stock, write_stock

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

  subroutine read_stock(filename, timestamps, open, high, &
    low, close, adjclose, volume)
    character(*), intent(in) :: filename  ! assumed-length input character string
    character(:), allocatable, intent(in out) :: timestamps(:) ! dynamic array of char strings of len 10
    real, allocatable, intent(in out) :: open(:), &       ! dynamic real arrays for stock data
       high(:), low(:), close(:), adjclose(:), volume(:)
    integer :: fileunit, n, nm

    ! finds the number of records (lines) in a file
    nm = num_records(filename) - 1

    ! allocate timestamps
    if (allocated(timestamps)) deallocate(timestamps)
    allocate(character(10) :: timestamps(nm))

    ! allocate stock price data
    call alloc(open, nm)
    call alloc(high, nm)
    call alloc(low, nm)
    call alloc(close, nm)
    call alloc(adjclose, nm)
    call alloc(volume, nm)

    ! opens the CSV file
    open(newunit=fileunit, file=filename)
    read(fileunit, fmt=*, end=1) ! skips the data header in first line
    do n = 1, nm
       ! reads the data line-by-line and stores into arrays
       read(fileunit, fmt=*, end=1) timestamps(n), open(n), &
          high(n), low(n), close(n), adjclose(n), volume(n)
    end do
    ! the 1 is a line label that Fortran uses if and when
    ! it encounters an exception in the read(fileunit, fmt=*, end=1)
    ! statements
    1 close(fileunit)
 end subroutine read_stock

 subroutine write_stock(filename, timestamps, price, mvavg, mvstd)
  ! Write derived stock data to file.
  character(len=*), intent(in) :: filename
  character(len=:), allocatable, intent(in) :: timestamps(:)
  real, intent(in) :: price(:), mvavg(:), mvstd(:)
  integer :: fileunit, n
  open(newunit=fileunit, file=filename)
  do n = 1, size(timestamps)
    write(fileunit, fmt=*) timestamps(n), price(n), mvavg(n), mvstd(n)
  end do
  close(fileunit)
 end subroutine write_stock

end module mod_io