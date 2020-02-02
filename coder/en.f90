program encoder
  implicit none

  integer :: ins
  integer :: line

  line = 1
  ins = 0

  do while (ins /= 2)
     read(*, "(B8.8)") ins

     write(*, "(A)", advance="no") char(ins)
     
     line = line + 1
  end do
end program encoder
