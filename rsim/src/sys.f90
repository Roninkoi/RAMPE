module sys
  implicit none

  integer*2 :: page = 0

  public :: r_idle
  public :: r_halt

  public :: r_pg
  public :: r_jmp
  public :: r_jez
contains
  ! system instructions
  subroutine r_idle()
    ! lel do nothing
  end subroutine r_idle

  subroutine r_halt(r)
    integer*2 :: r
    r = 0 ! stop
  end subroutine r_halt

  ! flow instructions
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
end module sys
