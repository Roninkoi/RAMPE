! RSIM - RAKIAC simulator
! simulates the RAKIAC CPU in software
! capable of running machine language from RASM
! command line use: rsim mode path
! mode = integer 0-2 (0 = from stdin,
! 1 = load from file, 2 = single step from file)
! path = relative path to .rexe executable
program rsim
  use sys
  use mem
  use reg
  use alu

  implicit none

  integer*16 :: i
  integer*16 :: t = 0 ! ticks
  integer*2 :: running = 1

  character(16) :: carg1, carg2
  character(16) :: si
  integer :: in = 0
  logical :: fi
  logical :: jumping
  
  real :: start, end

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

  do while (pc < pl .and. running == 1)
     if (in == 2) then ! single step
        read(*, *)
     end if
     if (in == 3) then
        call cpu_time(start)
        do while (end - start < 1.0/20.0) ! 20 Hz clock
           call cpu_time(end)
        end do
     end if

     ! cpu simulation start

     ! instruction fetching
     if (fi) then
        ir = fetch(pc) ! instruction
     else
        read(*, *) si ! from stdin
        call swrite(pc, si) ! write to mem
        ir = fetch(pc) ! instruction
     end if

     pc = pc + 1 ! inc pc

     ! instruction decoding
     call decode(ir) ! magic here

     ! cpu simulation end

     write(*, "('ticks: ' I0)") t

     if (checkof()) then
        print *, "Overflow!"
     end if

     if (running == 0) then
        print *, "Halt!"
     end if

     call printreg()

     t = t + 1
  end do

  call memdump()
contains
  subroutine decode(ins) ! instruction decoding
    use sys
    use mem
    use reg
    use alu

    implicit none

    integer*2 :: ins, i, a1, a2, v, v1, v3

    integer*2, pointer :: rp1, rp2

    i = ishft(ins, -4) ! extract 4- bit

    v = ishft(ins, 12) ! 4-bit
    v = ishft(v, -12)

    v1 = ishft(ins, 12) ! 1-bit
    v1 = ishft(v1, -15)

    v3 = ishft(ins, 13) ! 3-bit
    v3 = ishft(v3, -13)

    a1 = ishft(v, -2) ! 2-bit
    a2 = ishft(v, 14)
    a2 = ishft(a2, -14)

    select case (ins) ! system codes
    case (b'00000000')
       call r_nop()
    case (b'00000001')
       call r_hlt(running)
    case (b'00001000')
       call r_atm()
    case (b'00001001')
       call r_mta()
    case (b'00001010')
       call r_atp()
    case (b'00001011')
       call r_pta()
    case (b'00001100')
       call r_lda(a, mar)
    case (b'00001101')
       call r_sta(a, mar)
    case (b'00001110')
       call r_inc(a)
    case (b'00001111')
       call r_dec(a)
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
    case (b'0001') ! sw
       call r_sw(v, pc, mar)
    case (b'0010') ! jmp
       call r_jmp(v, pc)
    case (b'0011') ! jez
       call r_jez(v, pc, a)
    case (b'0100') ! jlz
       call r_jlz(v, pc, a)
    case (b'0101') ! mov
       call r_mov(rp1, rp2)
    case (b'0110') ! sto
       call r_sto(a, mar, v)
    case (b'0111') ! ld
       call r_ld(a, mar, v)
    case (b'1000') ! ldi
       call r_ldi(v, a)
    case (b'1001') ! not
       call r_not(rp1, a)
    case (b'1010') ! and
       call r_and(rp1, rp2, a)
    case (b'1011') ! or
       call r_or(rp1, rp2, a)
    case (b'1100') ! xor
       call r_xor(rp1, rp2, a)
    case (b'1101') ! add
       call r_add(rp1, rp2, a)
    case (b'1110') ! sub
       call r_sub(rp1, rp2, a)
    case (b'1111') ! sh
       call r_sh(a, v1, v3)
    end select

    nullify(rp1) ! pointers to null
    nullify(rp2)
  end subroutine decode
end program rsim
