module alu
  implicit none

  public :: r_not
  public :: r_and
  public :: r_or
  public :: r_xor
  public :: r_add
  public :: r_sub
  public :: r_rsh
  public :: r_lsh
contains
  subroutine r_not(a, acc) ! acc = !a
    integer*2 :: a, acc
    acc = not(a)
  end subroutine r_not

  subroutine r_and(a, b, acc) ! acc = a & b
    integer*2 :: a, b, acc
    acc = iand(a, b)
  end subroutine r_and

  subroutine r_or(a, b, acc) ! acc = a | b
    integer*2 :: a, b, acc
    acc = ior(a, b)
  end subroutine r_or

  subroutine r_xor(a, b, acc) ! acc = a ^ b
    integer*2 :: a, b, acc
    acc = ieor(a, b)
  end subroutine r_xor

  subroutine r_add(a, b, acc) ! acc = a + b
    integer*2 :: a, b, acc
    acc = a + b
  end subroutine r_add

  subroutine r_sub(a, b, acc) ! acc = a - b
    integer*2 :: a, b, acc
    acc = a - b
    if (acc < 0) then
       acc = 0 ! clamp sub
    end if
  end subroutine r_sub

  subroutine r_rsh(a, v, acc) ! acc  = a >> v
    integer*2 :: a, v, acc
    acc = ishft(a, -v)
  end subroutine r_rsh

  subroutine r_lsh(a, v, acc) ! acc = a << v
    integer*2 :: a, v, acc
    acc = ishft(a, v)
  end subroutine r_lsh
end module alu

