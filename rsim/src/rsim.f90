! RSIM - RAKIAC simulator
! simulates the RAKIAC CPU in software
! capable of running machine language from RASM
! command line use: rsim mode path
! mode = integer 0-2 (0 = run from stdin,
! 1 = run from file, 2 = single step from file,
! 3 = slow run from file, 4 = run quietly from file)
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
  logical :: fi, q
  logical :: jumping

  real :: start, end

  integer :: argc

  argc = iargc()

  if (argc < 1) then
     print *, "Usage: rsim <mode> <program.rexe>"
     return
  endif

  call getarg(1, carg1)
  call getarg(2, carg2)
  read(carg1, *) in
  fi = (in > 0)
  q = (in == 4)

  if (fi) then
     if (.not. q) then
        write(*, "(A, A)") "Loading program ", carg2
     endif
     
     call loadprog(trim(carg2))
  end if

  call initreg()

  if (.not. q) then
     write(*, "(A)") "Simulating RAKIAC..."
  endif

  do while (pc < pl .and. running == 1)
     if (in == 2) then ! single step
        read(*, *)
     end if
     if (in == 3) then
        call cpu_time(start)
        do while (end - start < 1.0/10.0) ! 10 Hz clock
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

     if (.not. q) then
        write(*, "('ticks: ' I0)") t

        if (checkof()) then
           print *, "Overflow!"
        end if

        if (running == 0) then
           print *, "Halt!"
        end if

        call printreg()
     endif

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
    case (int(b'00000000'))
       call r_nop()
    case (int(b'00000001'))
       call r_hlt(running)
    case (int(b'00001000'))
       call r_inc(a)
    case (int(b'00001001'))
       call r_dec(a)
    case (int(b'00001110'))
       call r_in(a)
    case (int(b'00001111'))
       call r_out(a)
    end select

    select case(a1) ! register pointer 1
    case (int(b'00'))
       rp1 => a
    case (int(b'01'))
       rp1 => b
    case (int(b'10'))
       rp1 => c
    case (int(b'11'))
       rp1 => d
    end select

    select case(a2) ! register pointer 2
    case (int(b'00'))
       rp2 => a
    case (int(b'01'))
       rp2 => b
    case (int(b'10'))
       rp2 => c
    case (int(b'11'))
       rp2 => d
    end select

    select case (i) ! instruction code
    case (int(b'0001'))
       call r_jmp(rp1, pc)
    case (int(b'0010'))
       call r_jez(rp1, rp2, pc)
    case (int(b'0011'))
       call r_jlz(rp1, rp2, pc)
    case (int(b'0100'))
       call r_mov(rp1, rp2)
    case (int(b'0101'))
       call r_sto(rp1, rp2)
    case (int(b'0110'))
       call r_ld(rp1, rp2)
    case (int(b'0111'))
       call r_ll(v, a)
    case (int(b'1000'))
       call r_lh(v, a)
    case (int(b'1001'))
       call r_not(rp1, a)
    case (int(b'1010'))
       call r_and(rp1, rp2, a)
    case (int(b'1011'))
       call r_or(rp1, rp2, a)
    case (int(b'1100'))
       call r_xor(rp1, rp2, a)
    case (int(b'1101'))
       call r_add(rp1, rp2, a)
    case (int(b'1110'))
       call r_sub(rp1, rp2, a)
    case (int(b'1111'))
       call r_sh(a, v1, v3)
    end select

    nullify(rp1) ! pointers to null
    nullify(rp2)
  end subroutine decode
end program rsim
