module alu
  implicit none

  integer*2 :: maxval = b'11111111'

  public :: r_inc
  public :: r_dec

  public :: r_not
  public :: r_and
  public :: r_or
  public :: r_xor
  public :: r_add
  public :: r_sub
  public :: r_sh
contains
  subroutine r_inc(acc) ! acc++
    integer*2 :: acc, nb

    acc = acc + 1

    acc = ishft(acc, 8)
    acc = ishft(acc, -8)
  end subroutine r_inc

  subroutine r_dec(acc) ! acc--
    integer*2 :: acc, nb

    nb = not(1) + 1 ! twos
    acc = acc + nb

    acc = ishft(acc, 8)
    acc = ishft(acc, -8)
  end subroutine r_dec

  subroutine r_not(a, acc) ! acc = !a
    integer*2 :: a, acc

    acc = not(a)
    acc = ishft(acc, 8)
    acc = ishft(acc, -8)
  end subroutine r_not

  subroutine r_and(a, b, acc) ! acc = a & b
    integer*2 :: a, b, acc

    acc = iand(a, b)
    acc = ishft(acc, 8)
    acc = ishft(acc, -8)
  end subroutine r_and

  subroutine r_or(a, b, acc) ! acc = a | b
    integer*2 :: a, b, acc

    acc = ior(a, b)
    acc = ishft(acc, 8)
    acc = ishft(acc, -8)
  end subroutine r_or

  subroutine r_xor(a, b, acc) ! acc = a ^ b
    integer*2 :: a, b, acc

    acc = ieor(a, b)
    acc = ishft(acc, 8)
    acc = ishft(acc, -8)
  end subroutine r_xor

  subroutine r_add(a, b, acc) ! acc = a + b
    integer*2 :: a, b, acc

    acc = a + b

    acc = ishft(acc, 8) ! get rid of half
    acc = ishft(acc, -8)
  end subroutine r_add

  subroutine r_sub(a, b, acc) ! acc = a - b
    integer*2 :: a, b, acc, nb

    nb = not(b) + 1 ! twos
    acc = a + nb

    acc = ishft(acc, 8)
    acc = ishft(acc, -8)
  end subroutine r_sub

  subroutine r_sh(acc, d, v) ! acc  = acc >> v
    integer*2 :: d, v, acc

    acc = ishft(acc, -((-1)**d)*(v))
    acc = ishft(acc, 8)
    acc = ishft(acc, -8)
  end subroutine r_sh
end module alu

