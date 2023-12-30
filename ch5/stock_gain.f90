program stock_gain
  
  ! use mod_arrays, only: reverse
  use mod_io, only: num_records
  use mod_alloc, only: alloc, free
  
  implicit none

  character(len=4), allocatable :: symbols(:)
  character(len=:), allocatable :: timestamps(:)
  real, allocatable :: open(:), high(:), low(:), &
                       close(:), adjclose(:), volume(:)

  integer :: n
  ! real :: gain

  symbols = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ', &
             'IMB ', 'INTC', 'MSFT', 'NVDA', 'ORCL']

  do n = 1, size(symbols)
    call read_stock( &
      'data/' // trim(symbols(n)) // '.csv', &
      timestamps, open, high, low, close, adjclose, volume)

  !   ! reverses the order of adjusted close price
  !   adjclose = reverse(adjclose)
  !   ! calculates the absolute stock gain
  !   gain = (adjclose(size(adjclose)) - adjclose(1))

  !   ! writes the table header to the screen, only on first iteration
  !   if (n == 1) then
  !     print *, &
  !       timestamps(size(timestamps)) // ' through ' // timestamps(1)
  !     print *, 'Symbol, Gain (USD), Relative gain (%)'
  !     print *, '-------------------------------------'
  !   end if

  !   ! calculates relative gain and prints results
  !   print *, symbols(n), gain, nint(gain / adjclose(1) * 100)

  end do

contains

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
  
end program stock_gain