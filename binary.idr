module binary

import public bool
import public byte2

||| Represents the addition of two bits and returns a byte
||| depending on the carry and plus value
export
half_adder: Bit -> Bit -> Byte2
half_adder b1 b0 = MkByte2 (bit_carry b1 b0) (bit_plus b1 b0)

||| Represents the addition of three bits and returns a byte value
||| depending on the carry and plus value.
export
full_adder: Bit -> Bit -> Bit -> Byte2
full_adder b1 b2 cin = MkByte2 (bit_carry3 b1 b2 cin) (bit_plus3 b1 b2 cin)
