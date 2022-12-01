! RSIM - RAMPE simulator
! simulates the RAMPE CPU in software
! capable of running machine language from RASM
! command line use: rsim [mode] [file]
! mode = -s (single step), -c (clock), -q (run quietly)
! file = relative path to .rexe executable
! rsim (0 = run from stdin)
! rsim file (1 = run from file)
! rsim -s file (2 = single step from file)
! rsim -c file (3 = run from file using clock)
! rsim -q file (4 = run quietly from file)
program rsim
  use sys
  use mem
  use reg
  use alu

  implicit none

  integer*16 :: i
  integer*16 :: t = 0 ! cycle
  integer*2 :: running = 1

  character(16) :: carg1, carg2
  character(16) :: si
  integer :: in = 0
  logical :: fi, q
  logical :: jumping = .false.

  real :: start, end, clockspd

  integer :: argc

  argc = iargc()

  clockspd = 10

  call getarg(1, carg1)
  call getarg(2, carg2)

  if (argc < 1) then
     !print *, "Usage: rsim <mode> <program.rexe>"
     in = 0
  else if (argc == 1) then
     in = 1
     carg2 = carg1
  else if (argc == 2) then
     if (carg1 == "-s") then
        in = 2
     end if
     if (carg1 == "-c") then
        in = 3
     end if
     if (carg1 == "-q") then
        in = 4
     end if
  end if
  !print *, argc, carg1, carg2, in
  !return

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
     write(*, "(A)") "Simulating RAMPE..."
  endif

  start = 0
  end = 0

  do while (pc < pl .and. running == 1)
     if (in == 2) then ! single step
        read(*, *)
     end if
     if (in == 3) then
        call cpu_time(start)
        do while (end - start < 1.0/clockspd) ! clockspd Hz clock
           call cpu_time(end)
        end do
     end if

     ! cpu simulation start

     ! instruction fetching
     ir = fetch(pc, fi)
     
     ! instruction decoding
     call decode(ir) ! magic here

     if (.not. jumping) then
        call incpc(pc) ! increment program counter
     end if

     ! cpu simulation end

     if (.not. q) then
        write(*, "('cycle: ' I0)") (t+1)

        if (checkof()) then
           print *, "Overflow!"
        end if

        if (running == 0) then
           print *, "Halt!"
        end if

        call printreg()
        if (bank > 0) then
           write(*, "('bank: ' B8.8 ' ('I0')')") bank, bank
        end if
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

    integer*2 :: ins, i, a1, a2, v, m2, m4

    integer*2, pointer :: rp1, rp2

    i = ishft(ins, -4) ! extract 4- bit

    m4 = int(b'00001111')
    v = iand(ins, m4) ! 4-bit

    m2 = int(b'00000011')
    a1 = ishft(v, -2)
    a2 = iand(ins, m2)

    jumping = .false.
    
    select case (ins) ! system codes
    case (int(b'00000000'))
       call r_nop()
    case (int(b'00000001'))
       call r_hlt(running)
    case (int(b'00001000'))
       call r_inc(a)
    case (int(b'00001001'))
       call r_dec(a)
    case (int(b'00001010'))
       call r_get(a, pc)
    case (int(b'00001011'))
       call r_set(a, pc)
    case (int(b'00001100'))
       jumping = r_sw(a, b, bank, pc)
    case (int(b'00001110'))
       if (.not. q) print *, "in:"
       call r_in(a)
    case (int(b'00001111'))
       if (.not. q) print *, "out:"
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
       jumping = r_jmp(rp1, pc)
    case (int(b'0010'))
       jumping = r_jez(rp1, rp2, pc)
    case (int(b'0011'))
       jumping = r_jlz(rp1, rp2, pc)
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
       call r_not(rp1)
    case (int(b'1010'))
       call r_and(rp1, rp2)
    case (int(b'1011'))
       call r_or(rp1, rp2)
    case (int(b'1100'))
       call r_xor(rp1, rp2)
    case (int(b'1101'))
       call r_add(rp1, rp2)
    case (int(b'1110'))
       call r_sub(rp1, rp2)
    case (int(b'1111'))
       call r_sh(rp2, a1)
    end select

    nullify(rp1) ! pointers to null
    nullify(rp2)
  end subroutine decode
end program rsim
