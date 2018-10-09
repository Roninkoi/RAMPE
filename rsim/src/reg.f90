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

  integer*2 :: maxval = b'11111111'

  public :: initreg
  public :: printreg
  public :: getbits
contains
  subroutine initreg()
    a =  b'00000000' ! init registers
    b =  b'00000000'
    c =  b'00000000'
    d =  b'00000000'
    ir = b'00000000'
    pc = b'00000000'
  end subroutine initreg

  subroutine printreg()
    write(*, "('a:  ' B8.8 ' ('I0')')") a, a
    write(*, "('b:  ' B8.8 ' ('I0')')") b, b
    write(*, "('c:  ' B8.8 ' ('I0')')") c, c
    write(*, "('d:  ' B8.8 ' ('I0')')") d, d
    write(*, "('pc: ' B8.8 ' ('I0')')") pc, pc
    write(*, "('ir: ' B8.8 ' ('I0')')") ir, ir
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
end module reg
