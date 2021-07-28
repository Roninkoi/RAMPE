module reg
  implicit none

  ! general purpose registers
  integer(kind = 2), target :: a ! accumulator
  integer(kind = 2), target :: b
  integer(kind = 2), target :: c
  integer(kind = 2), target :: d

  ! special purpose registers
  integer(kind = 2) :: pc ! program counter
  integer(kind = 2) :: ir ! instruction register

  integer*2 :: maxval = int(b'11111111')

  public :: initreg
  public :: printreg
  public :: getbits

  public :: r_mov
  public :: r_ll
  public :: r_lh
contains
  subroutine initreg()
    a   = int(b'00000000') ! init registers
    b   = int(b'00000000')
    c   = int(b'00000000')
    d   = int(b'00000000')
    ir  = int(b'00000000')
    pc  = int(b'00000000')
  end subroutine initreg

  integer function tc2d(v) ! twos complement to decimal
    integer*2 :: v, val, sgn

    sgn = ishft(v, -7)
    val = ishft(v, 9)
    val = ishft(val, -9)

    if (sgn == 1) then
       val = not(val)
       val = val + 1
       val = 128 + val
    end if

    tc2d = ((-1)**sgn)*val
  end function tc2d

  subroutine printreg()
    write(*, "('a:   ' B8.8 ' ('I0')')") a, tc2d(a)
    write(*, "('b:   ' B8.8 ' ('I0')')") b, tc2d(b)
    write(*, "('c:   ' B8.8 ' ('I0')')") c, tc2d(c)
    write(*, "('d:   ' B8.8 ' ('I0')')") d, tc2d(d)

    write(*, "('ir:  ' B8.8 ' ('I0')')") ir, ir
    write(*, "('pc:  ' B8.8 ' ('I0')')") pc, pc
  end subroutine printreg

  logical function checkof()
    checkof = (a > maxval .or. b > maxval .or. &
         c > maxval .or. d > maxval .or. &
         ir > maxval .or. pc > maxval)
  end function checkof

  function getbits(r)
    integer*2 :: r
    integer*2 :: getbits(8)

    getbits = [ishft(r, -7), &
         ishft(ishft(r, 1), -7), &
         ishft(ishft(r, 2), -7), &
         ishft(ishft(r, 3), -7), &
         ishft(ishft(r, 4), -7), &
         ishft(ishft(r, 5), -7), &
         ishft(ishft(r, 6), -7), &
         ishft(ishft(r, 7), -7)]

    where(getbits >= 1)
       getbits = 1
    elsewhere
       getbits = 0
    end where
    return
  end function getbits

  subroutine r_out(a)
    integer*2 :: a
    write(*, '(I0)') tc2d(a)
  end subroutine r_out

  subroutine r_in(a)
    integer*2 :: a
    read(*, '(I8)') a
  end subroutine r_in

  ! register instructions
  subroutine r_mov(reg1, reg2) ! move
    integer*2, pointer :: reg1, reg2
    reg1 = reg2
  end subroutine r_mov

  subroutine r_ll(val, acc) ! load literal value to lower 4 bits
    integer*2 :: val, acc, mask
    mask = int(b'11110000')
    acc = iand(acc, mask)
    acc = acc + val
  end subroutine r_ll

  subroutine r_lh(val, acc) ! load value to higher 4 bits
    integer*2 :: val, acc, mask
    mask = int(b'1111')
    acc = iand(acc, mask)
    acc = ishft(val, 4) + acc
  end subroutine r_lh
end module reg
