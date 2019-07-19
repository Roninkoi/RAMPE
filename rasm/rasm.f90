! RASM - RAKIAC assembler
! translates RASM into RAKIAC machine language
! command line use: rasm input.rasm output.rexe
program rasm
  implicit none

  character(16) :: carg1, carg2
  character(32) :: ins, ml
  character(32) :: op, a1, a2
  character(32) :: instructions(256) ! prog max 256
  integer :: line
  integer :: adr, bnk

  character(32) :: labels(32)
  integer :: ladr(32)
  integer :: li

  character(32) :: labelc
  integer :: labeli

  real :: start, end, time

  call getarg(1, carg1)
  call getarg(2, carg2)

  open(8, file = carg1) ! in
  open(9, file = carg2) ! out

  print *, "rasm source: ", carg1
  print *, "rexe output: ", carg2
  print *, ""

  call cpu_time(start)

  line = 1
  li = 1
  labeli = 1
  ins = ""

  do while (ins /= "end") ! preprocessing pass
     read(8, "(A)") ins
     ins = trim(ins)
     instructions(line) = ins

     bnk = ishft(line-1, -4)
     adr = line - ishft(bnk, 4) - 1

     call label(ins, labelc, li) ! memory labeling

     if (labelc /= "") then
        labels(li-1) = labelc
        ladr(li-1) = adr
     end if

     line = line + 1
  end do
  
  line = 1
  ins = ""

  do while (ins /= "end") ! instruction parsing
     ins = instructions(line)

     bnk = ishft(line-1, -4)
     adr = line - ishft(bnk, 4) - 1

     call parse(ins, op, a1, a2)

     labeli = 1
     do while (labeli < li) ! label substitution
        if (a1 == labels(labeli)) then
           a1 = itoc(ladr(labeli))
        end if
        if (a2 == labels(labeli)) then
           a2 = itoc(ladr(labeli))
        end if
        labeli = labeli + 1
     end do

     ml = ops(op, a1, a2) ! machine translation

     write(*, "(I4 ' | ' I2 ', '  I2 ' |  ' A10A)") line, bnk, adr, ml, ins
     write(9, "(A8)") ml

     line = line + 1
  end do

  call cpu_time(end)

  time = end - start

  print *, ""
  print *, "labels:"

  labeli = 1
  do while (labeli < li)
     write(*, "(A10 ' = ' I4)") labels(labeli), ladr(labeli)
     labeli = labeli + 1
  end do

  print *, ""
  print *, "Assembly completed in: ", time, " s"

  close(8)
  close(9)
contains
  subroutine label(in, lab, l)
    character(32), intent(in) :: in
    character(32), intent(out) :: lab
    integer, intent(inout) :: l

    logical :: ns, ons
    character :: c
    integer :: i

    i = 1
    ns = .true.
    ons = .true.

    lab = ""

    do while (i <= 32)
       c = in(i:i)

       ons = ns

       ns = c /= " " .and. c /= "," &
            .and. c /= "\r" .and. c /= "\n" &
            .and. c /= "\r\n"

       if (c == ";") then ! comment, end parsing
          return
       end if

       if (c == ":") then ! this was a label, reset

          ! save label and address

          !print *, lab
          !print *, la

          lab = in(1:i-1)

          l = l + 1

          c = ""
          ns = .false.
          ons = .true.
       end if

       i = i + 1
    end do
  end subroutine label

  subroutine parse(ins, op, a1, a2)
    character(32) :: ins
    character(32) :: op, a1, a2

    logical :: ns, ons, istart
    character :: c
    integer :: i, word

    op = ""
    a1 = ""
    a2 = ""
    i = 1
    word = 0
    ns = .true.
    ons = .true.

    istart = .false.

    do while (i <= 32 .and. word < 3)
       c = ins(i:i)

       ons = ns

       ns = c /= " " .and. c /= "," &
            .and. c /= "\r" .and. c /= "\n" &
            .and. c /= "\r\n"

       if (c == ";") then ! comment, end parsing
          return
       end if

       if (ns .and. .not. ons .and. istart) then
          word = word + 1
       end if

       if (c == ":") then ! label, reset
          word = 0
          c = ""
          ns = .false.
          ons = .true.
          istart = .false.
          op = ""
          a1 = ""
          a2 = ""
       end if

       if (ns) then
          istart = .true.
          select case (word)
          case (0)
             op = trim(op) // c
          case (1)
             a1 = trim(a1) // c
          case (2)
             a2 = trim(a2) // c
          end select
       end if

       i = i + 1
    end do
  end subroutine parse

  function ctob4(c) result(b) ! char to bit 4
    character(32) :: c
    character(32) :: b
    integer :: i

    read(c, *) i
    write(b, "(B4.4)") i   
  end function ctob4

  function ctob3(c) result(b) ! char to bit 3
    character(32) :: c
    character(32) :: b
    integer :: i

    read(c, *) i
    write(b, "(B3.3)") i   
  end function ctob3

  function ctob2(c) result(b) ! char to bit 2
    character(32) :: c
    character(32) :: b
    integer :: i

    read(c, *) i
    write(b, "(B2.2)") i   
  end function ctob2

  function ctob1(c) result(b) ! char to bit 1
    character(32) :: c
    character(32) :: b
    integer :: i

    read(c, *) i
    write(b, "(B1.1)") i   
  end function ctob1

  function ctoi(c) result(b) ! char to integer
    character(32) :: c
    integer :: b
    integer :: i

    read(c, *) i
    b = i
  end function ctoi

  function itoc(i) result(b) ! integer to char
    character(32) :: c
    character(32) :: b
    integer :: i

    write(c, *) i
    b = c
  end function itoc

  function rtob2(r) result(b) ! register name to bit 2
    character(32) :: r
    character(32) :: b

    select case(r)
    case ("a")
       b = "00"
    case ("b")
       b = "01"
    case ("c")
       b = "10"
    case ("d")
       b = "11"
    end select
  end function rtob2

  function ops(op, a1, a2) result(ml)
    character(32) :: op, a1, a2
    character(32) :: ml
    character(32) :: ac1, ac2
    integer :: ai1, ai2

    select case (op)
    case ("nop")
       ml = "00000000"
    case ("hlt")
       ml = "00000001"
    case ("end")
       ml = "00000010"
    case ("atm")
       ml = "00001000"
    case ("mta")
       ml = "00001001"
    case ("atp")
       ml = "00001010"
    case ("pta")
       ml = "00001111"
    case ("lda")
       ml = "00001100"
    case ("sta")
       ml = "00001101"
    case ("inc") ! increment
       ml = "00001110"
    case ("dec") ! decrement
       ml = "00001111"
    case ("sw")
       ml = "0001"
       ml = trim(ml) // trim(ctob4(a1))
    case ("jmp")
       ml = "0010"
       ml = trim(ml) // trim(ctob4(a1))
    case ("jez")
       ml = "0011"
       ml = trim(ml) // trim(ctob4(a1))
    case ("jlz")
       ml = "0100"
       ml = trim(ml) // trim(ctob4(a1))
    case ("mov")
       ml = "0101"
       ml = trim(ml) // trim(rtob2(a1))
       ml = trim(ml) // trim(rtob2(a2))
    case ("sto")
       ml = "0110"
       ml = trim(ml) // trim(ctob4(a1))
    case ("ld")
       ml = "0111"
       ml = trim(ml) // trim(ctob4(a1))
    case ("ldi")
       ml = "1000"
       ml = trim(ml) // trim(ctob4(a1))
    case ("not")
       ml = "1001"
       ml = trim(ml) // trim(rtob2(a1))
       ml = trim(ml) // "00"
    case ("and")
       ml = "1010"
       ml = trim(ml) // trim(rtob2(a1))
       ml = trim(ml) // trim(rtob2(a2))
    case ("or")
       ml = "1011"
       ml = trim(ml) // trim(rtob2(a1))
       ml = trim(ml) // trim(rtob2(a2))
    case ("xor")
       ml = "1100"
       ml = trim(ml) // trim(rtob2(a1))
       ml = trim(ml) // trim(rtob2(a2))
    case ("add")
       ml = "1101"
       ml = trim(ml) // trim(rtob2(a1))
       ml = trim(ml) // trim(rtob2(a2))
    case ("sub")
       ml = "1110"
       ml = trim(ml) // trim(rtob2(a1))
       ml = trim(ml) // trim(rtob2(a2))
    case ("sh")
       ml = "1111"
       ml = trim(ml) // trim(ctob1(a1))
       ml = trim(ml) // trim(ctob3(a2))
    case ("shr") ! shift right
       ml = "11110"
       ml = trim(ml) // trim(ctob3(a1))
    case ("shl") ! shift left
       ml = "11111"
       ml = trim(ml) // trim(ctob3(a1))
    case ("")
       ml = "00000000"
    case default
       ml = op
    end select
  end function ops
end program rasm
