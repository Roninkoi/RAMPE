! RSIM - RAKR simulator
! simulates the RAKR cpu in software
! capable of running machine language from rasm
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

  write(*, "(A)") "Simulating RAKR..."

  do while (t < pl .and. running == 1)
     write(*, "('ticks: ' I0)") t

     ! cpu simulation start

     if (fi) then
        ir = fetch(pc) ! instruction
        pc = pc + 1
        ar = fetch(pc) ! arg
        pc = pc + 1
     else
        read(*, *) si
        call swrite(pc, si)
        ir = fetch(pc) ! instruction
        pc = pc + 1
        read(*, *) si
        call swrite(pc, si)
        ar = fetch(pc) ! arg
        pc = pc + 1
     end if

     call decode(ir, ar)

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
  subroutine decode(ins, arg) ! instruction decoding
    use reg
    use sys
    use alu

    implicit none

    integer*2 :: ins, arg, a1, a2

    integer*2, pointer :: rp1, rp2

    a1 = ishft(arg, -4) ! extract 4-bit
    a2 = ishft(arg, 12)
    a2 = ishft(a2, -12)

    select case (ins) ! system code
    case (b'00000000')
       call r_idle()
    case (b'00000001')
       call r_halt(running)
    end select

    select case(a1) ! register pointer 1
    case (b'00000000')
       rp1 => a
    case (b'00000001')
       rp1 => b
    case (b'00000010')
       rp1 => c
    case (b'00000011')
       rp1 => d
    end select

    select case(a2) ! register pointer 2
    case (b'00000000')
       rp2 => a
    case (b'00000001')
       rp2 => b
    case (b'00000010')
       rp2 => c
    case (b'00000011')
       rp2 => d
    end select

    select case (ins) ! instruction code
    case (b'01000010') ! jmp
       call r_jmp(arg, pc)
    case (b'01000011') ! jez
       call r_jez(arg, pc, a)
    case (b'01000100') ! mov
       call r_mov(rp1, rp2)
    case (b'01000101') ! movr
       call r_movr(arg, a)
    case (b'01000110') ! mova
       call r_mova(arg, a)
    case (b'01000111') ! movl
       call r_movl(arg, a)
    case (b'10001000') ! not
       call r_not(rp1, a)
    case (b'10001001') ! and
       call r_and(rp1, rp2, a)
    case (b'10001010') ! or
       call r_or(rp1, rp2, a)
    case (b'10001011') ! xor
       call r_xor(rp1, rp2, a)
    case (b'10001100') ! add
       call r_add(rp1, rp2, a)
    case (b'10001101') ! sub
       call r_sub(rp1, rp2, a)
    case (b'10001110') ! rsh
       call r_rsh(rp1, rp2, a)
    case (b'10001111') ! lsh
       call r_lsh(rp1, rp2, a)
    end select

    nullify(rp1) ! pointers to null
    nullify(rp2)
  end subroutine decode
end program rsim
