module mem
  implicit none

  ! program in bytes
  character(8) :: p(0:255) = "00000000"
  integer*2 :: pl = 255 ! program length

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
    character(8) :: s

    open(8, file = path)

    do while (s /= "00000010") ! end
       read(8, *) s ! bytes?

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
  subroutine r_sto(address, acc) ! store accumulator
    integer*2 :: address, acc
    call iwrite(address, acc)
  end subroutine r_sto

  subroutine r_ld(address, acc) ! load address
    integer*2 :: address
    integer*2 ::  acc
    acc = fetch(address)
  end subroutine r_ld
end module mem
