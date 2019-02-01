module sys
  implicit none

  public :: r_nop
  public :: r_hlt

  public :: r_sw
  public :: r_jmp
  public :: r_jez
  public :: r_mta
  public :: r_atm
  public :: r_pta
  public :: r_atp
contains
  ! system instructions
  subroutine r_nop()
    ! idle
  end subroutine r_nop

  subroutine r_hlt(r)
    integer*2 :: r
    r = 0 ! stop
  end subroutine r_hlt

  subroutine r_atm(a, mar) ! acc to mar
    integer*2 :: a, mar

    mar = a
  end subroutine r_atm

  subroutine r_mta(a, mar) ! mar to acc
    integer*2 :: a, mar

    a = mar
  end subroutine r_mta

  subroutine r_atp(a, pc) ! acc to pc
    integer*2 :: a, pc

    pc = a
  end subroutine r_atp

  subroutine r_pta(a, pc) ! pc to acc
    integer*2 :: a, pc

    a = pc
  end subroutine r_pta

  ! flow instructions
  subroutine r_sw(b, pc, mar) ! memory bank switching
    integer*2 :: b, pc, mar, bank
    
    bank = ishft(b, 4) ! high
    
    pc = ishft(pc, 12) ! address
    pc = ishft(pc, -12)
    pc = bank + pc
    
    mar = ishft(mar, 12) ! address
    mar = ishft(mar, -12)
    mar = bank + mar
  end subroutine r_sw

  subroutine r_jmp(address, pc) ! jump
    integer*2 :: address, pc

    pc = ishft(pc, -4)
    pc = ishft(pc, 4)
    pc = pc + address
  end subroutine r_jmp

  subroutine r_jez(address, pc, acc) ! jump equal zero
    integer*2 :: address, pc, acc
    
    if (acc == 0) then
       pc = ishft(pc, -4)
       pc = ishft(pc, 4)
       pc = pc + address
    end if
  end subroutine r_jez

  subroutine r_jlz(address, pc, acc) ! jump less zero
    integer*2 :: address, pc, acc, accs
    
    accs = ishft(acc, -7)
    
    if (accs == 1) then
       pc = ishft(pc, -4)
       pc = ishft(pc, 4)
       pc = pc + address
    end if
  end subroutine r_jlz
end module sys
