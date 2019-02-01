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
  integer(kind = 2) :: mar ! memory address register

  integer*2 :: maxval = b'11111111'

  public :: initreg
  public :: printreg
  public :: getbits

  public :: r_mov
  public :: r_ldi
contains
  subroutine initreg()
    a   = b'00000000' ! init registers
    b   = b'00000000'
    c   = b'00000000'
    d   = b'00000000'
    ir  = b'00000000'
    pc  = b'00000000'
    mar = b'00000000'
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
    write(*, "('mar: ' B8.8 ' ('I0')')") mar, mar
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

  ! register instructions
  subroutine r_mov(reg1, reg2) ! move
    integer*2, pointer :: reg1, reg2
    reg1 = reg2
  end subroutine r_mov

  subroutine r_ldi(val, acc) ! load direct immediate (literal)
    integer*2 :: val, acc
    acc = val
  end subroutine r_ldi
end module reg
