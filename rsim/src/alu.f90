module alu
  implicit none

  integer*2 :: maxval = int(b'11111111')

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
    acc = iand(acc, maxval)
  end subroutine r_inc

  subroutine r_dec(acc) ! acc--
    integer*2 :: acc, nb

    nb = not(1) + 1 ! twos
    acc = acc + nb
    acc = iand(acc, maxval)
  end subroutine r_dec

  subroutine r_not(a) ! a = !a
    integer*2 :: a

    a = not(a)
    a = iand(a, maxval)
  end subroutine r_not

  subroutine r_and(a, b) ! a = a & b
    integer*2 :: a, b

    a = iand(a, b)
  end subroutine r_and

  subroutine r_or(a, b) ! a = a | b
    integer*2 :: a, b

    a = ior(a, b)
  end subroutine r_or

  subroutine r_xor(a, b) ! a = a ^ b
    integer*2 :: a, b

    a = ieor(a, b)
  end subroutine r_xor

  subroutine r_add(a, b) ! a = a + b
    integer*2 :: a, b

    a = a + b
    a = iand(a, maxval)
  end subroutine r_add

  subroutine r_sub(a, b) ! a = a - b
    integer*2 :: a, b, nb

    nb = not(b) + 1 ! twos
    a = a + nb
    a = iand(a, maxval)
  end subroutine r_sub

  subroutine r_sh(a, v) ! acc  = acc >> v
    integer*2 :: a, v, s

    s = 0
    select case (v)
       case (0)
          s = -2
       case (1)
          s = -1
       case (2)
          s = 1
       case (3)
          s = 2
    end select

    a = ishft(a, s)
    a = iand(a, maxval)
  end subroutine r_sh
end module alu

