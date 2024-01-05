program stock_volatility

   use mod_arrays, only: average, std, moving_average, &
      moving_std, reverse
   use mod_io, only: read_stock, write_stock

   implicit none

   character(len=4), allocatable :: symbols(:)
   character(len=:), allocatable :: timestamps(:)
   real, allocatable :: open(:), high(:), low(:), &
      close(:), adjclose(:), volume(:)

   integer :: i, im, n
   
   symbols = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ', &
      'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']

   do n = 1, size(symbols)
      call read_stock( &
         'data2018/' // trim(symbols(n)) // '.csv', &
         timestamps, open, high, low, close, adjclose, volume)

      im = size(timestamps)
      adjclose = reverse(adjclose)

      ! writes the table header to the screen, only on first iteration
      if (n == 1) then
         print *, &
            timestamps(size(timestamps)) // ' through ' // timestamps(1)
         print *, 'Symbol, Average (USD), Volatility (USD), Relative Volatility (%)'
         print *, '----------------------------------------------------------------'
      end if

      print *, symbols(n), average(adjclose), std(adjclose),&
      nint(std(adjclose) / average(adjclose) * 100)

      timestamps = timestamps(im:1:-1)

      call write_stock(trim(              &
        symbols(n)) // '_volatility.txt', &
        timestamps, adjclose,    &
        moving_average(adjclose, 30),     &
        moving_std(adjclose, 30))

   end do

end program stock_volatility
