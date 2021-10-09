module mem
  implicit none

  ! program in bytes
  character(8) :: p(0:255) = "00000000"
  integer*2 :: pl = 256 ! program length

  public :: loadprog
  public :: fetch
  public :: iwrite
  public :: swrite

  public :: r_sto
  public :: r_ld
contains
  subroutine loadprog(path) ! load from file
    character(16) :: path
    integer :: c = 0, i = 0
    integer :: io
    character(8) :: s

    open(8, file = path, iostat = io)

    do while (i < pl)
       read(8, *, iostat = io) s ! bytes?

       if (io /= 0) exit

       p(i) = s

       i = i + 1
    end do
    close(8)
  end subroutine loadprog

  function fetch(i) ! fetch instruction from memory
    integer*2 :: i
    integer*2 :: c = 0
    integer*2 :: fetch

    read(p(i), "(B8.8)") c
    fetch = c
    return
  end function fetch

  subroutine iwrite(a, v)
    integer*2 :: v, a

    write(p(a), "(B8.8)") v
  end subroutine iwrite

  subroutine swrite(a, v)
    character(8) :: v
    integer*2 :: a

    write(p(a), "(A)") v
  end subroutine swrite

  subroutine memdump()
    open(9, file = "out.rmem")
    write(9, "(A)") p
  end subroutine memdump

  ! mem instructions
  subroutine r_sto(a, b) ! store accumulator
    integer*2 :: a, b

    call iwrite(b, a)
  end subroutine r_sto

  subroutine r_ld(a, b) ! load accumulator
    integer*2 :: a, b

    a = fetch(b)
  end subroutine r_ld

  subroutine r_get(a, pc)
    integer*2 :: a, pc

    a = fetch(pc)
  end subroutine r_get

  subroutine r_set(a, pc)
    integer*2 :: a, pc

    call iwrite(pc, a)
  end subroutine r_set
end module mem
