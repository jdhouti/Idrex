module byte4

import public bit
import public nat
import ifthenelse
import byte2

public export
data Byte4 = MkByte4 Bit Bit Bit Bit

export
byte4_new: Bit -> Bit -> Bit -> Bit -> Byte4
byte4_new b0 b1 b2 b3 = MkByte4 b0 b1 b2 b3

export
byte4_zeros: Byte4
byte4_zeros = MkByte4 B0 B0 B0 B0

export
byte4_ones: Byte4
byte4_ones = MkByte4 B1 B1 B1 B1

||| Returns the right most bit
export
byte4_bit0: Byte4 -> Bit
byte4_bit0 (MkByte4 b3 b2 b1 b0) = b0

||| Returns the next bit
export
byte4_bit1: Byte4 -> Bit
byte4_bit1 (MkByte4 b3 b2 b1 b0) = b1

||| Returns the next bit
export
byte4_bit2: Byte4 -> Bit
byte4_bit2 (MkByte4 b3 b2 b1 b0) = b2

||| Returns the first bit (highest order)
export
byte4_bit3: Byte4 -> Bit
byte4_bit3 (MkByte4 b3 b2 b1 b0) = b3

||| Addition of a 2 4 bit bytes
export
byte4_plus: Byte4 -> Byte4 -> Byte4
byte4_plus (MkByte4 b1 b2 b3 b4) (MkByte4 c1 c2 c3 c4) =
  MkByte4
    (bit_plus3
      b1
      c1
      (bit_carry3
        b2
        c2
        (bit_carry3
          b3
          c3
          (bit_carry3
            B0
            b4
            c4))))
    (bit_plus3
      b2
      c2
      (bit_carry3
        b3
        c3
        (bit_carry
          b4
          c4)))
    (bit_plus3
      b3
      c3
      (bit_carry
        b4
        c4))
    (bit_plus3
      B0
      b4
      c4)

||| Return the indexed bit of the given byte or B0 if the index is out of range
export
byte4_sub: Nat -> Byte4 -> Bit
byte4_sub index aByte4 =
  ifthenelse
    (nat_eq index nat_zero)
    (byte4_bit0 aByte4)
    (ifthenelse
      (nat_eq index nat_one)
      (byte4_bit1 aByte4)
      (ifthenelse
        (nat_eq index nat_two)
        (byte4_bit2 aByte4)
        (ifthenelse
          (nat_eq index nat_three)
          (byte4_bit3 aByte4)
          (B0))))

||| Returns the bit sum of
carry_sub: Nat -> Byte4 -> Byte4 -> Bit
carry_sub n b1 b0 =
  ifthenelse
    (nat_eq n nat_zero)
    (B0)
    (bit_carry3
      (carry_sub (nat_pred n) b1 b0)
      (byte4_sub (nat_pred n) b1)
      (byte4_sub (nat_pred n) b0))

||| Return the indexed bit of the sum of the given bytes
sum_sub: Nat -> Byte4 -> Byte4 -> Bit
sum_sub n b1 b0 =
  bit_plus3
    (carry_sub n b1 b0)
    (byte4_sub n b1)
    (byte4_sub n b0)

||| Return the sum of the two 4-bit byte ignoring overflows.
export
byte4_plus': Byte4 -> Byte4 -> Byte4
byte4_plus' b0 b1 =
  MkByte4
    (sum_sub nat_three b1 b0)
    (sum_sub nat_two   b1 b0)
    (sum_sub nat_one   b1 b0)
    (sum_sub nat_zero  b1 b0)

||| Return TRUE if both byte4s are identical. Return FALSE if anything else
export
byte4_eq: Byte4 -> Byte4 -> Bool
byte4_eq (MkByte4 b0 b1 b2 b3) (MkByte4 b4 b5 b6 b7) =
  bool_and
    (byte2_eq (MkByte2 b0 b1) (MkByte2 b4 b5))
    (byte2_eq (MkByte2 b2 b3) (MkByte2 b6 b7))

export
implementation Eq Byte4 where
  eq b1 b2 = byte4_eq b1 b2
  neq b1 b2 = bool_not (eq b1 b2)
