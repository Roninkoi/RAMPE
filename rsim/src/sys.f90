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
  logical function r_jmp(a, pc) ! jump
    integer*2 :: a, pc

    pc = a

    r_jmp = .true.
  end function r_jmp

  logical function r_jez(a, b, pc) ! jump equal zero
    integer*2 :: a, b, pc
    logical :: jumping = .false.
    
    if (a == 0) then
       pc = b
       jumping = .true.
    end if
    r_jez = jumping
  end function r_jez

  logical function r_jlz(a, b, pc) ! jump less zero
    integer*2 :: a, b, pc, as
    logical :: jumping = .false.
    
    as = ishft(a, -7)
    
    if (as == 1) then
       pc = b
       jumping = .true.
    end if
    r_jlz = jumping
  end function r_jlz
end module sys
