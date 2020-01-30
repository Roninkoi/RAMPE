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
    case (b'00000100')
       call r_in(a)
    case (b'00001000')
       call r_atm()
    case (b'00001001')
       call r_mta()
    case (b'00001010')
       call r_atp()
    case (b'00001011')
       call r_pta()
    case (b'00001100')
       call r_lda(a, mar)
    case (b'00001101')
       call r_sta(a, mar)
    case (b'00001110')
       call r_inc(a)
    case (b'00001111')
       call r_dec(a)
    end select

    select case(a1) ! register pointer 1
    case (b'00')
       rp1 => a
    case (b'01')
       rp1 => b
    case (b'10')
       rp1 => c
    case (b'11')
       rp1 => d
    end select

    select case(a2) ! register pointer 2
    case (b'00')
       rp2 => a
    case (b'01')
       rp2 => b
    case (b'10')
       rp2 => c
    case (b'11')
       rp2 => d
    end select

    select case (i) ! instruction code
    case (b'0001') ! sw
       call r_sw(v, pc, mar)
    case (b'0010') ! jmp
       call r_jmp(v, pc)
    case (b'0011') ! jez
       call r_jez(v, pc, a)
    case (b'0100') ! jlz
       call r_jlz(v, pc, a)
    case (b'0101') ! mov
       call r_mov(rp1, rp2)
    case (b'0110') ! sto
       call r_sto(a, mar, v)
    case (b'0111') ! ld
       call r_ld(a, mar, v)
    case (b'1000') ! ldi
       call r_ldi(v, a)
    case (b'1001') ! not
       call r_not(rp1, a)
    case (b'1010') ! and
       call r_and(rp1, rp2, a)
    case (b'1011') ! or
       call r_or(rp1, rp2, a)
    case (b'1100') ! xor
       call r_xor(rp1, rp2, a)
    case (b'1101') ! add
       call r_add(rp1, rp2, a)
    case (b'1110') ! sub
       call r_sub(rp1, rp2, a)
    case (b'1111') ! sh
       call r_sh(a, v1, v3)
    end select

    nullify(rp1) ! pointers to null
    nullify(rp2)
  end subroutine decode
end program rsim
