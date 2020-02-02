program decoder
  implicit none

  character(3000) :: prog
  integer :: ins
  integer :: line

  line = 1
  ins = 0
  
  read(*, "(A)") prog
     
  do while (ins /= 538976258)
     read(prog(line:line+1), "(A)") ins

     write(*, "(B8.8)") char(ins)
     
     line = line + 1
  end do
end program decoder
