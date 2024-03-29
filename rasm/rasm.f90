! RASM - RAMPE assembler
! translates RASM into RAMPE machine language
! command line use: rasm <input.rasm> <output.rexe>
program rasm
  implicit none

  integer, parameter :: maxcols = 64 ! maximum length of line
  integer, parameter :: pl = 65536 ! program length
  integer :: argc
  character(80) :: carg1, carg2
  character(maxcols) :: ins, ml ! instruction, machine code
  character(maxcols) :: op, a1, a2 ! opcode, args
  character(maxcols) :: instructions(pl) ! program
  integer :: val, io
  integer :: line, lines ! current line, number of lines
  integer :: adr, adrh, bnk, bnkh ! address, bank

  character(maxcols) :: labelc ! label string
  character(maxcols) :: labels(pl) ! list of labels
  integer :: ladr(pl) ! low address label
  integer :: ladrh(pl) ! high address label
  integer :: lbnk(pl) ! low bank label
  integer :: lbnkh(pl) ! high bank label
  integer :: labeli ! first pass label index, number of labels
  integer :: li ! label index

  real :: start, end, time

  argc = iargc()

  if (argc < 1) then
     print *, "Usage: rasm <input.rasm> <output.rexe>"
     return
  endif

  call getarg(1, carg1)
  call getarg(2, carg2)

  open(8, file = carg1) ! in
  open(9, file = carg2) ! out

  print *, "rasm source: ", carg1
  print *, "rexe output: ", carg2
  print *, ""

  call cpu_time(start)

  line = 1
  labeli = 1
  ins = ""
  io = 0

  do while (.true.) ! preprocessing pass
     read(8, "(A)", iostat = io) ins
     if (io < 0) then
        exit
     end if
     ins = adjustl(trim(ins))
     instructions(line) = ins

     call parse(ins, op, a1, a2)

     bnkh = ishft(line-1, -12)
     bnk = ishft(line-1, -8) - ishft(bnkh, 4)
     adrh = ishft(line-1, -4) - ishft(bnk, 4) - ishft(bnkh, 8)
     adr = line - 1 - ishft(adrh, 4) - ishft(bnk, 8) - ishft(bnkh, 12)
     
     call label(ins, labelc, labeli) ! memory labeling
     
     if (op == "la") then
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
     end if
     if (op == "lb") then
        instructions(line) = "ll " // "@" // a1
        line = line + 1
        instructions(line) = "lh " // "@" // a1
     end if
     if (op == "lba") then
        instructions(line) = "ll " // "@" // a1
        line = line + 1
        instructions(line) = "lh " // "@" // a1
        line = line + 1
        instructions(line) = "mov b, a"
        line = line + 1
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
     end if
     if (op == "ja") then
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "jmp a"
     end if
     if (op == "jea") then
        instructions(line) = "mov b, a"
        line = line + 1
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "jez b, a"
     end if
     if (op == "jla") then
        instructions(line) = "mov b, a"
        line = line + 1
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "jlz b, a"
     end if
     if (op == "jeb") then
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "jez b, a"
     end if
     if (op == "jlb") then
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "jlz b, a"
     end if
     if (op == "jec") then
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "jez c, a"
     end if
     if (op == "jlc") then
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "jlz c, a"
     end if
     if (op == "jed") then
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "jez d, a"
     end if
     if (op == "jld") then
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "jlz d, a"
     end if
     if (op == "lda") then
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "ld a, a"
     end if
     if (op == "ldb") then
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "ld b, a"
     end if
     if (op == "ldc") then
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "ld c, a"
     end if
     if (op == "ldd") then
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "ld d, a"
     end if
     if (op == "sta") then
        instructions(line) = "mov b, a"
        line = line + 1
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "sto b, a"
     end if
     if (op == "stb") then
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "sto b, a"
     end if
     if (op == "stc") then
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "sto c, a"
     end if
     if (op == "std") then
        instructions(line) = "ll " // a1
        line = line + 1
        instructions(line) = "lh " // a1
        line = line + 1
        instructions(line) = "sto d, a"
     end if
     if (op == "shr") then
        instructions(line) = "sh 1, " // a1
     end if
     if (op == "shl") then
        instructions(line) = "sh 2, " // a1
     end if
     if (op == "shrr") then
        instructions(line) = "sh 0, " // a1
     end if
     if (op == "shll") then
        instructions(line) = "sh 3, " // a1
     end if

     ! read op as integer
     read(op, '(I8)', iostat=io) val
     ! if valid decimal, convert to binary
     if (io == 0 .and. len(trim(op)) < 8 .and. len(trim(op)) > 0) then
        write(op, "(B8.8)") val
        instructions(line) = op
     end if

     if (len(trim(op)) == 0) then ! remove empty lines
        line = line - 1
     end if

     if (labelc /= "") then
        labels(labeli-1) = trim(labelc)
        ladrh(labeli-1) = adrh
        ladr(labeli-1) = adr
        lbnkh(labeli-1) = bnkh
        lbnk(labeli-1) = bnk
     end if

     line = line + 1
  end do

  lines = line - 1
  line = 1
  ins = ""
  io = 0

  do while (line <= lines) ! instruction parsing
     ins = instructions(line)

     bnkh = ishft(line-1, -12)
     bnk = ishft(line-1, -8) - ishft(bnkh, 4)
     adrh = ishft(line-1, -4) - ishft(bnk, 4) - ishft(bnkh, 8)
     adr = line - 1 - ishft(adrh, 4) - ishft(bnk, 8) - ishft(bnkh, 12)

     call parse(ins, op, a1, a2)

     li = 1
     do while (li < labeli) ! label substitution
        if (a1 == labels(li)) then
           if (op == "lh") then ! label high address
              a1 = itoc(ladrh(li))
           else
              a1 = itoc(ladr(li))
           end if
        end if
        if (a1 == "@" // labels(li)) then
           if (op == "lh") then ! label high bank
              a1 = itoc(lbnkh(li))
           else
              a1 = itoc(lbnk(li))
           end if
        end if
        if (a2 == labels(li)) then
           a2 = itoc(ladr(li))
        end if
        li = li + 1
     end do

     ml = ops(op, a1, a2) ! machine translation
     
     read(ml, '(B8)', iostat=io) val
     if (io /= 0 .or. len(trim(ml)) /= 8) then
        print *, "Not a valid instruction:", ml
     end if

     write(*, "(I5 ' | ' I2 ', ' I2 ', ' I2 ', ' I2 ' | ' A10A)") line, bnkh, bnk, adrh, adr, ml, trim(ins)
     write(9, "(A8)") ml

     line = line + 1
  end do

  call cpu_time(end)

  time = end - start

  print *, ""
  print *, "labels:"

  li = 1
  do while (li < labeli)
     write(*, "(A10 ' = ' I2 ', ' I2 ', ' I2 ', ' I2)") labels(li), lbnkh(li), lbnk(li), ladrh(li), ladr(li)
     li = li + 1
  end do

  print *, ""
  print *, "Assembly completed in: ", time, " s"

  close(8)
  close(9)
contains
  subroutine label(in, lab, l)
    character(maxcols), intent(in) :: in
    character(maxcols), intent(out) :: lab
    integer, intent(inout) :: l

    logical :: ns, ons
    character :: c
    integer :: i

    i = 1
    ns = .true.
    ons = .true.

    lab = ""

    do while (i <= maxcols)
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

          lab = adjustl(trim(in(1:i-1)))

          l = l + 1

          c = ""
          ns = .false.
          ons = .true.
       end if

       i = i + 1
    end do
  end subroutine label

  subroutine parse(ins, op, a1, a2)
    character(maxcols) :: ins
    character(maxcols) :: op, a1, a2

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

    do while (i <= maxcols .and. word < 3)
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
    character(maxcols) :: c
    character(maxcols) :: b
    integer :: i

    read(c, *, iostat=io) i
    if (io /= 0) print *, "Invalid binary:", c
    write(b, "(B4.4)") i
  end function ctob4

  function ctob3(c) result(b) ! char to bit 3
    character(maxcols) :: c
    character(maxcols) :: b
    integer :: i

    read(c, *, iostat=io) i
    write(b, "(B3.3)") i
    if (io /= 0) print *, "Invalid binary:", c
  end function ctob3

  function ctob2(c) result(b) ! char to bit 2
    character(maxcols) :: c
    character(maxcols) :: b
    integer :: i

    read(c, *, iostat=io) i
    if (io /= 0) print *, "Invalid binary:", c
    write(b, "(B2.2)") i   
  end function ctob2

  function ctob1(c) result(b) ! char to bit 1
    character(maxcols) :: c
    character(maxcols) :: b
    integer :: i

    read(c, *, iostat=io) i
    if (io /= 0) print *, "Invalid binary:", c
    write(b, "(B1.1)") i   
  end function ctob1

  function ctoi(c) result(b) ! char to integer
    character(maxcols) :: c
    integer :: b
    integer :: i = 0

    read(c, *, iostat=io) i
    if (io /= 0) print *, "Invalid integer:", c
    b = i
  end function ctoi

  function itoc(i) result(b) ! integer to char
    character(maxcols) :: c = ""
    character(maxcols) :: b
    integer :: i

    write(c, *) i
    b = c
  end function itoc

  function rtob2(r) result(b) ! register name to bit 2
    character(maxcols) :: r
    character(maxcols) :: b

    b = "00"
    select case(r)
    case ("a")
       b = "00"
    case ("b")
       b = "01"
    case ("c")
       b = "10"
    case ("d")
       b = "11"
    case default
       print *, "Couldn't find register:", r
    end select
  end function rtob2

  function ops(op, a1, a2) result(ml)
    character(maxcols) :: op, a1, a2
    character(maxcols) :: ml
    character(maxcols) :: ac1, ac2
    integer :: ai1, ai2

    select case (op)
    case ("nop")
       ml = "00000000"
    case ("hlt")
       ml = "00000001"
    case ("inc")
       ml = "00001000"
    case ("dec")
       ml = "00001001"
    case ("get")
       ml = "00001010"
    case ("set")
       ml = "00001011"
    case ("sw")
       ml = "00001100"
    case ("in")
       ml = "00001110"
    case ("out")
       ml = "00001111"
    case ("jmp")
       ml = "0001"
       ml = trim(ml) // trim(rtob2(a1))
       ml = trim(ml) // "00"
    case ("jez")
       ml = "0010"
       ml = trim(ml) // trim(rtob2(a1))
       ml = trim(ml) // trim(rtob2(a2))
    case ("jlz")
       ml = "0011"
       ml = trim(ml) // trim(rtob2(a1))
       ml = trim(ml) // trim(rtob2(a2))
    case ("mov")
       ml = "0100"
       ml = trim(ml) // trim(rtob2(a1))
       ml = trim(ml) // trim(rtob2(a2))
    case ("sto")
       ml = "0101"
       ml = trim(ml) // trim(rtob2(a1))
       ml = trim(ml) // trim(rtob2(a2))
    case ("ld")
       ml = "0110"
       ml = trim(ml) // trim(rtob2(a1))
       ml = trim(ml) // trim(rtob2(a2))
    case ("ll")
       ml = "0111"
       ml = trim(ml) // trim(ctob4(a1))
    case ("lh")
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
       ml = trim(ml) // trim(ctob2(a1))
       ml = trim(ml) // trim(rtob2(a2))
    case ("")
       ml = "00000000"
    case default
       ml = op
    end select
  end function ops
end program rasm
