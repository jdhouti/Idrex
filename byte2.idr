module byte2

{-
A byte is a tuple of bits. Users of the
Byte type will need to have access to the
definitions for the Bit type. We add the
keyword "public" to the import of the bit
module to tell Idris to export them to accordingly
users of the byte module.
-}
import public bit
import public bool
import eq

{-
We define a byte as a tuple of two bits
tagged by the MkByte constructor. We will
refer to the right bit as "bit zero" and
the left bit as "bit one." We also call them
the "low order" (b0, right) and "high-order"
bit (b1, left).
-}
public export
data Byte2 = MkByte2 Bit Bit

{-
Now we meet a very important idea: a pattern
can "pull apart a term/value and bind names to
its parts." The next two functions use this idea
to pull apart a byte, revealing its constitutent
bits, in order to return its bit zero and bit one
components. We call such a function a "projection
function," an "accessor," or an "inspector."
-}

||| Constructs a byte with 2 bits based on the given 2 bits
export
byte2_b0: Byte2 -> Bit
byte2_b0 (MkByte2 b1 b0) = b0

export
byte2_b1: Byte2 -> Bit
byte2_b1 (MkByte2 b1 b0) = b1

||| Adds two byte2s using deconstruction.
export
byte2_plus: Byte2 -> Byte2 -> Byte2
byte2_plus (MkByte2 b0 b1) (MkByte2 b2 b3) =
  MkByte2
    (bit_plus b1 b3)
    (bit_plus3 b0 b2 (bit_carry b1 b3))

||| Returns true if two byte2s are the same, false if anything else
export
byte2_eq: Byte2 -> Byte2 -> Bool
byte2_eq (MkByte2 b0 b1) (MkByte2 b2 b3) =
  bool_and (bit_eq b0 b2) (bit_eq b1 b3)

||| Tests whether two given byte2s are equal.
||| Return true if they are and false if they are not.
export
implementation Eq Byte2 where
  eq b1 b2 = byte2_eq b1 b2
  neq b1 b2 = bool_not (eq b1 b2)
