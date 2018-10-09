
module mem
  implicit none

  ! program in bytes
  character(8) :: p(0:255) = "00000000"
  integer*2 :: pl = 255 ! program length

  public :: loadprog
  public :: fetch
  public :: iwrite
  public :: swrite
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
end module mem

module sys
  use mem

  implicit none

  integer*2 :: page = 0

  public :: r_idle
  public :: r_halt

  public :: r_pg
  public :: r_jmp
  public :: r_jez
  public :: r_mov
  public :: r_sto
  public :: r_ld
  public :: r_movl
contains
  ! system instructions
  subroutine r_idle()
    ! lel do nothing
  end subroutine r_idle

  subroutine r_halt(r)
    integer*2 :: r
    r = 0 ! stop
  end subroutine r_halt

  ! func instructions
  subroutine r_pg(pg, pc) ! page flip
    integer*2 :: pg, pc, address
    address = ishft(pc, 12) ! address
    address = ishft(address, -12)
    page = ishft(pg, 4) ! high
    pc = page + address
  end subroutine r_pg

  subroutine r_jmp(address, pc) ! jump
    integer*2 :: address, pc
    pc = page + address
  end subroutine r_jmp

  subroutine r_jez(address, pc, acc) ! jump equal zero
    integer*2 :: address, pc, acc
    if (acc == 0) then
       pc = page + address
    end if
  end subroutine r_jez

  subroutine r_mov(reg1, reg2) ! move
    integer*2, pointer :: reg1, reg2
    reg1 = reg2
  end subroutine r_mov

  subroutine r_sto(address, acc) ! store accumulator
    integer*2 :: address, acc
    call iwrite(address, acc)
  end subroutine r_sto

  subroutine r_ld(address, acc) ! load address
    integer*2 :: address
    integer*2 ::  acc
    acc = fetch(address)
  end subroutine r_ld

  subroutine r_movl(val, acc) ! move value
    integer*2 :: val, acc
    acc = val
  end subroutine r_movl
end module sys

