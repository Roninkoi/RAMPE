module sys
  implicit none

  public :: r_nop
  public :: r_hlt

  public :: r_jmp
  public :: r_jez
  public :: r_jlz
contains  
  ! system instructions
  subroutine r_nop()
    ! idle
  end subroutine r_nop

  subroutine r_hlt(r)
    integer*2 :: r
    r = 0 ! stop
  end subroutine r_hlt

  ! flow instructions
  subroutine r_jmp(a, pc) ! jump
    integer*2 :: a, pc

    pc = a
  end subroutine r_jmp

  subroutine r_jez(a, b, pc) ! jump equal zero
    integer*2 :: a, b, pc
    
    if (a == 0) then
       pc = b
    end if
  end subroutine r_jez

  subroutine r_jlz(a, b, pc) ! jump less zero
    integer*2 :: a, b, pc, as
    
    as = ishft(a, -7)
    
    if (as == 1) then
       pc = b
    end if
  end subroutine r_jlz
end module sys
