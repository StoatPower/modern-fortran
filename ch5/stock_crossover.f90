program stock_crossover

  use mod_arrays, only: crossneg, crosspos, reverse
  use mod_io, only: read_stock

  implicit none

  character(len=4), allocatable :: symbols(:)
  character(len=:), allocatable :: timestamps(:)
  real, allocatable :: open(:), high(:), low(:), close(:), adjclose(:), volume(:)
  integer :: fileunit, i, im, n
  integer, allocatable :: buy(:), sell(:)

  symbols = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ',&
             'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']

  do n = 1, size(symbols)

    print *, 'Processing moving average crossover for ' // symbols(n)

    call read_stock('data2018/' // trim(symbols(n)) // '.csv', timestamps, &
      open, high, low, close, adjclose, volume)

    ! timestamps is an array of strings, so we have to use the slice
    timestamps = timestamps(size(timestamps):1:-1)
    adjclose = reverse(adjclose)

    ! finds positive and negative crossover
    buy = crosspos(adjclose, 30)
    sell = crossneg(adjclose, 30)

    open(newunit=fileunit, file=trim(symbols(n)) // '_crossover.txt')
    do i = 1, size(buy)
      write(fileunit, fmt=*) 'Buy ', timestamps(buy(i))
    end do
    do i = 1, size(sell)
      write(fileunit, fmt=*) 'Sell ', timestamps(sell(i))
    end do
    close(fileunit)

  end do

end program stock_crossover