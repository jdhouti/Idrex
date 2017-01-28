||| Abstract data type representing two-bit byte
module byte8

{-
A byte aggregates several bits into a tuple of bits.
The bits are essential, thus visible, parts of a byte.
Note that this is a comment. You cannot attach inline
documentation to an import directive.
-}
import public bit
import public nat
import ifthenelse
import byte4

||| A two-bit byte type; constructor is private
export
data Byte8 =
  ||| Constructor that boxes four Bit values into a Byte8 value
  MkByte8 Bit Bit Bit Bit Bit Bit Bit Bit

||| Pack four bits into, and return, a byte (a 8-tuple of bits)
export
byte8_new: Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Byte8
byte8_new b7 b6 b5 b4 b3 b2 b1 b0 = MkByte8 b7 b6 b5 b4 b3 b2 b1 b0


-- Constant byte8 values filled with zeros and ones, resp.

export
byte8_zeros: Byte8
byte8_zeros = MkByte8 B0 B0 B0 B0 B0 B0 B0 B0


export
byte8_ones: Byte8
byte8_ones = MkByte8 B1 B1 B1 B1 B1 B1 B1 B1

-- Projection functions, returning particular component bits

||| Return Bit 0
export
byte8_bit0: Byte8 -> Bit
byte8_bit0 (MkByte8 b7 b6 b5 b4 b3 b2 b1 b0) = b0

||| Return Bit 1
export
byte8_bit1: Byte8 -> Bit
byte8_bit1 (MkByte8 b7 b6 b5 b4 b3 b2 b1 b0) = b1

||| Return Bit 2
export
byte8_bit2: Byte8 -> Bit
byte8_bit2 (MkByte8 b7 b6 b5 b4 b3 b2 b1 b0) = b2

||| Return Bit 3
export
byte8_bit3: Byte8 -> Bit
byte8_bit3 (MkByte8 b7 b6 b5 b4 b3 b2 b1 b0) = b3


||| Return Bit 3
export
byte8_bit4: Byte8 -> Bit
byte8_bit4 (MkByte8 b7 b6 b5 b4 b3 b2 b1 b0) = b4


||| Return Bit 3
export
byte8_bit5: Byte8 -> Bit
byte8_bit5 (MkByte8 b7 b6 b5 b4 b3 b2 b1 b0) = b5


||| Return Bit 3
export
byte8_bit6: Byte8 -> Bit
byte8_bit6 (MkByte8 b7 b6 b5 b4 b3 b2 b1 b0) = b6


||| Return Bit 3
export
byte8_bit7: Byte8 -> Bit
byte8_bit7 (MkByte8 b7 b6 b5 b4 b3 b2 b1 b0) = b7


-- arithmetic functions on 8-bit bytes

-- natural numbers we'll need
{-
nat_four: Nat
nat_four = nat_succ nat_three

nat_five: Nat
nat_five = nat_succ nat_four

nat_six: Nat
nat_six = nat_succ nat_five

nat_seven: Nat
nat_seven = nat_succ nat_six
-}

||| Return the indexed bit of the given byte or B0 if the index is out of range
export
byte8_sub: Nat -> Byte8 -> Bit
byte8_sub index aByte8 =
  ifthenelse
    (nat_eq index nat_zero)
    (byte8_bit0 aByte8)
    (ifthenelse
      (nat_eq index nat_one)
      (byte8_bit1 aByte8)
      (ifthenelse
        (nat_eq index nat_two)
        (byte8_bit2 aByte8)
        (ifthenelse
          (nat_eq index nat_three)
          (byte8_bit3 aByte8)
          (ifthenelse
            (nat_eq index nat_four)
            (byte8_bit4 aByte8)
            (ifthenelse
              (nat_eq index nat_five)
              (byte8_bit5 aByte8)
              (ifthenelse
                (nat_eq index nat_six)
                (byte8_bit6 aByte8)
                (ifthenelse
                  (nat_eq index nat_seven)
                  (byte8_bit7 aByte8)
                  (B0))))))))


||| Return the carry bit in the indexed column when adding the two bytes
||| or the zero bit (B0) if the index is out of range
carry_sub8: Nat -> Byte8 -> Byte8 -> Bit
carry_sub8 n b1 b0 =
  ifthenelse
    (nat_eq n nat_zero)
    (B0)
    (bit_carry3
      (carry_sub8 (nat_pred n) b1 b0)
      (byte8_sub (nat_pred n) b1)
      (byte8_sub (nat_pred n) b0))


||| Return the indexed bit of the sum of the given bytes
sum_sub8: Nat -> Byte8 -> Byte8 -> Bit
sum_sub8 n b1 b0 =
  bit_plus3
    (carry_sub8 n b1 b0)
    (byte8_sub n b1)
    (byte8_sub n b0)


||| Return the sum of two 8-bit bytes as a 8-bit byte ignoring overflows
export
byte8_plus: Byte8 -> Byte8 -> Byte8
byte8_plus b0 b1 =
  MkByte8
    (sum_sub8 nat_zero  b0 b1)
    (sum_sub8 nat_one   b0 b1)
    (sum_sub8 nat_two   b0 b1)
    (sum_sub8 nat_three b0 b1)
    (sum_sub8 nat_four  b0 b1)
    (sum_sub8 nat_five  b0 b1)
    (sum_sub8 nat_six   b0 b1)
    (sum_sub8 nat_seven b0 b1)


||| Returns TRUE if both byte8s are identical. Returns FALSE if anything else.
export
byte8_eq: Byte8 -> Byte8 -> Bool
byte8_eq
  (MkByte8 b0 b1 b2 b3 b4 b5 b6 b7)
  (MkByte8 b8 b9 b10 b11 b12 b13 b14 b15) =
    bool_and
      (byte4_eq (MkByte4 b0 b1 b2 b3) (MkByte4 b8 b9 b10 b11))
      (byte4_eq (MkByte4 b4 b5 b6 b7) (MkByte4 b12 b13 b14 b15))

-------------------------------------------------------------------------------
--        OPTIONAL EXAM CODE CAN BE FOUND BELOW WITH EXPLANATIONS            --
-------------------------------------------------------------------------------

||| Note: I have attached bit.idr to my submission because I have created important functions
||| in it that I use in the next upcoming functions.

||| created bit_not in bit.idr that takes the negation of a single bit
||| please view bit.idr (attached) on lines 66 and 67 for function details
export
byte8_not: Byte8 -> Byte8
byte8_not
  (MkByte8 b0 b1 b2 b3 b4 b5 b6 b7) =
    (MkByte8
      (bit_not b0)        -- takes the negation of b0
      (bit_not b1)        -- takes the negation of b1
      (bit_not b2)        -- takes the negation of b2
      (bit_not b3)        -- takes the negation of b3
      (bit_not b4)        -- takes the negation of b4
      (bit_not b5)        -- takes the negation of b5
      (bit_not b6)        -- takes the negation of b6
      (bit_not b7))       -- takes the negation of b7

-- note: the above function could have also been created if I had implemented my
-- bit negation function on byte4 and simply done it for both sides of the byte8
-- instead of doing this for every single bit in the given byte8.

||| This function uses both bit_new and bit_eq
||| I created bit_new in bit.idr in order to be able to create a bit in a different
||| file other than bit.idr (since MkBit is reserved for the bit.idr file)
||| bit_new does the same thing as MkBit and can be found on lines 19 and 20 of bit.idr
export
byte8_and: Byte8 -> Byte8 -> Byte8
byte8_and
  (MkByte8 b0 b1 b2 b3 b4 b5 b6 b7)
  (MkByte8 b8 b9 b10 b11 b12 b13 b14 b15) =
    (MkByte8
      (bit_new (bit_eq b0 b8))      -- checks if b0 and b8 are equal and returns the equivalent boolean
      (bit_new (bit_eq b1 b9))      -- which is then converted to a bit using my function bit_new
      (bit_new (bit_eq b2 b10))
      (bit_new (bit_eq b3 b11))     -- this is applied to every corresponding bit pair in the given byte8s
      (bit_new (bit_eq b4 b12))
      (bit_new (bit_eq b5 b13))
      (bit_new (bit_eq b6 b14))
      (bit_new (bit_eq b7 b15)))

||| created the bit_or function in bit.idr on lines 72 and 73
||| bit_or simply converts both bits into bools and returns a boolean based on that
||| it then converts that boolean into a bit
||| works the same way as bool_or, except bit_or is for bits.
export
byte8_or: Byte8 -> Byte8 -> Byte8
byte8_or
  (MkByte8 b0 b1 b2 b3 b4 b5 b6 b7)
  (MkByte8 b8 b9 b10 b11 b12 b13 b14 b15) =
    (MkByte8
      (bit_or b0 b8)      -- bit_new is not used here because bit_or creates the
      (bit_or b1 b9)      -- bit for you.
      (bit_or b2 b10)
      (bit_or b3 b11)
      (bit_or b4 b12)
      (bit_or b5 b13)
      (bit_or b6 b14)
      (bit_or b7 b15))

-- the function below is NOT part of the optional exam.
export
implementation Eq Byte8 where
  eq b1 b2 = byte8_eq b1 b2
  neq b1 b2 = bool_not (eq b1 b2)
