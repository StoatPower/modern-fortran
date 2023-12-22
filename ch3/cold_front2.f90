program cold_front

  implicit none

  integer :: n 
  real :: nhours  ! time interval

  do n = 6, 48, 6
    nhours = real(n)
    print *, 'Temperature after ', nhours, ' hours is ', &
      cold_front_temperature(12., 24., 20., 960., nhours), ' degrees.'
  end do

contains

  real function cold_front_temperature(temp1, temp2, c, dx, dt) result(res)
    real, intent(in) :: temp1, temp2, c, dx, dt
    res = temp2 - c * (temp2 - temp1) / dx * dt
  end function cold_front_temperature

end program cold_front