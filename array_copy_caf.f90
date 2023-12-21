program array_copy_caf

  implicit none

  ! declares and initializes an integer coarray
  integer :: array(5)[*] = 0
  integer, parameter :: sender = 1, receiver = 2

  ! throws error if we're not running on 2 processes
  if (num_images() /= 2) &
    stop 'Error: This program must be run on 2 parallel processes'

  ! initializes array in sender
  if (this_image() == sender) array = [1, 2, 3, 4, 5]

  print '(a,i2,a,5(4x,i2))', 'array on proc', this_image(), &
    ' before copy:', array

  ! waits here for all images; equivalent to mpi_barrier()
  sync all

  ! nonblocking copy from sending image to receiving image
  if (this_image() == receiver) &
    array(:) = array(:)[sender]

  print '(a,i1,a,5(4x,i2))', 'array on proc ', this_image(), &
    ' after copy: ', array

end program array_copy_caf