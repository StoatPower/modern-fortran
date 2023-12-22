program cold_front

  implicit none

  real :: temp1 = 12, temp2 = 24
  real :: dx = 960, c = 20, dt = 24
  real :: res ! result in deg. C

  res = temp2 - c * (temp2 - temp1) / dx * dt

  print *, 'Temperature after ', dt, 'hours is ', res, 'degrees'

end program cold_front