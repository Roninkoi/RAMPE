! RSIM - RAKIAC simulator
! simulates the RAKIAC CPU in software
! capable of running machine language from RASM
! command line use: rsim mode path
! mode = integer 0-2 (0 = dynamic,
! 1 = load from file, 2 = single step from file)
! path = relative path to .rexe executable
program rsim
  use reg
  use alu
  use mem
  use sys

  implicit none

  integer*16 :: i
  integer*2 :: t = 0 ! ticks
  integer*2 :: running = 1

  character(16) :: carg1, carg2
  character(16) :: si
  integer :: in = 0
  logical :: fi

  call getarg(1, carg1)
  call getarg(2, carg2)
  read(carg1, *) in
  fi = (in > 0)

  if (fi) then
     write(*, "(A)") "Loading program..."
     call loadprog(trim(carg2))
  end if

  call initreg()

  write(*, "(A)") "Simulating RAKIAC..."

  do while (t < pl .and. running == 1)
     write(*, "('ticks: ' I0)") t

     ! cpu simulation start

     if (fi) then
        ir = fetch(pc) ! instruction
        pc = pc + 1
     else
        read(*, *) si
        call swrite(pc, si)
        ir = fetch(pc) ! instruction
        pc = pc + 1
     end if

     call decode(ir)

     if (in == 2) then ! single step
        read(*, *)
     end if

     if (checkof()) then
        print *, "Overflow!"
     end if

     if (running == 0) then
        print *, "Halt!"
     end if

     ! cpu simulation end

     call printreg()

     t = t + 1
  end do
  call memdump()
contains
  subroutine decode(ins) ! instruction decoding
    use reg
    use sys
    use alu

    implicit none

    integer*2 :: ins, i, a1, a2, v

    integer*2, pointer :: rp1, rp2

    i = ishft(ins, -4)

    v = ishft(ins, 12)
    v = ishft(v, -12)
    
    a1 = ishft(v, -2) ! extract 2-bit
    a2 = ishft(v, 14)
    a2 = ishft(a2, -14)

    select case (ins) ! system code
    case (b'00000000')
       call r_idle()
    case (b'00000001')
       call r_halt(running)
    end select

    select case(a1) ! register pointer 1
    case (b'00')
       rp1 => a
    case (b'01')
       rp1 => b
    case (b'10')
       rp1 => c
    case (b'11')
       rp1 => d
    end select

    select case(a2) ! register pointer 2
    case (b'00')
       rp2 => a
    case (b'01')
       rp2 => b
    case (b'10')
       rp2 => c
    case (b'11')
       rp2 => d
    end select

    select case (i) ! instruction code
    case (b'0001') ! pg
       call r_pg(v, pc)
    case (b'0010') ! jmp
       call r_jmp(v, pc)
    case (b'0011') ! jez
       call r_jez(v, pc, a)
    case (b'0100') ! mov
       call r_mov(rp1, rp2)
    case (b'0101') ! sto
       call r_sto(v, a)
    case (b'0110') ! ld
       call r_ld(v, a)
    case (b'0111') ! movl
       call r_movl(v, a)
    case (b'1000') ! not
       call r_not(rp1, a)
    case (b'1001') ! and
       call r_and(rp1, rp2, a)
    case (b'1010') ! or
       call r_or(rp1, rp2, a)
    case (b'1011') ! xor
       call r_xor(rp1, rp2, a)
    case (b'1100') ! add
       call r_add(rp1, rp2, a)
    case (b'1101') ! sub
       call r_sub(rp1, rp2, a)
    case (b'1110') ! rsh
       call r_rsh(rp1, a2, a)
    case (b'1111') ! lsh
       call r_lsh(rp1, a2, a)
    end select

    nullify(rp1) ! pointers to null
    nullify(rp2)
  end subroutine decode
end program rsim
