program stock_gain

   use mod_arrays, only: reverse
   use mod_io, only: read_stock
   use mod_alloc, only: alloc, free

   implicit none

   character(len=4), allocatable :: symbols(:)
   character(len=:), allocatable :: timestamps(:)
   real, allocatable :: open(:), high(:), low(:), &
      close(:), adjclose(:), volume(:)

   integer :: n
   ! Absolute gain: diff bt last/first adjusted close price
   ! Tells us how much the stock price grew over a period of time;
   ! doesn't tell us anything about whether the growth is small or
   ! large relative to the stock price itself
   real :: gain

   symbols = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ', &
      'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']

   do n = 1, size(symbols)
      call read_stock( &
         'data2018/' // trim(symbols(n)) // '.csv', &
         timestamps, open, high, low, close, adjclose, volume)

      ! reverses the order of adjusted close price
      adjclose = reverse(adjclose)
      ! calculates the absolute stock gain
      gain = (adjclose(size(adjclose)) - adjclose(1))

      ! writes the table header to the screen, only on first iteration
      if (n == 1) then
         print *, &
            timestamps(size(timestamps)) // ' through ' // timestamps(1)
         print *, 'Symbol, Gain (USD), Relative gain (%)'
         print *, '-------------------------------------'
      end if

      ! calculates relative gain and prints results
      print *, symbols(n), gain, nint(gain / adjclose(1) * 100)

   end do

end program stock_gain
