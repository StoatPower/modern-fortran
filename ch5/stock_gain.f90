program stock_gain
  
  use mod_arrays, only: reverse
  use mod_io, only: read_stock
  
  implicit none

  character(len=4), allocatable :: symbols(:)
  character(len=:), allocatable :: time(:)
  real, allocatable :: open(:), high(:), low(:), &
                       close(:), adjclose(:), volume(:)

  integer :: n
  real :: gain

  symbols = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ', &
             'IMB ', 'INTC', 'MSFT', 'NVDA', 'ORCL']

  do n = 1, size(symbols)
    call read_stock( &
      'data/' // trim(symbols(n)) // '.csv', &
      time, open, high, low, close, adjclose, volume)

    ! reverses the order of adjusted close price
    adjclose = reverse(adjclose)
    ! calculates the absolute stock gain
    gain = (adjclose(size(adjclose)) - adjclose(1))

    ! writes the table header to the screen, only on first iteration
    if (n == 1) then
      print *, &
        time(size(time)) // ' through ' // time(1)
      print *, 'Symbol, Gain (USD), Relative gain (%)'
      print *, '-------------------------------------'
    end if

    ! calculates relative gain and prints results
    print *, symbols(n), gain, nint(gain / adjclose(1) * 100)

  end do
  
end program stock_gain