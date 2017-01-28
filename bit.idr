module bit

import public bool
import eq

{- https://upload.wikimedia.org/wikipedia/commons/5/57/Fulladder.gif -}

export
data Bit = MkBit Bool

||| Defines B0 as a bit equivalent to a false bool
export
B0: Bit
B0 = MkBit False

||| created for byte8.idr optional EXAM
||| used to construct a bit in a different .idr file
export
bit_new: Bool -> Bit
bit_new b = (MkBit b)

||| Defines B1 as a bit equivalent to a true bool
export
B1: Bit
B1 = MkBit True

||| Converts a given bit to a bool
export
bit_rep: Bit -> Bool
bit_rep (MkBit b) = b

||| Represents the sum of two bits by first converting two booleans to bits
export
bit_plus: Bit -> Bit -> Bit
bit_plus (MkBit b1) (MkBit b2) = MkBit (bool_xor b1 b2)

||| Represents the carry value when adding two bits
export
bit_carry: Bit -> Bit -> Bit
bit_carry (MkBit b1) (MkBit b2) = MkBit (bool_and b1 b2)

||| Represents the addition of 3 bits by converting all first three bools to bits
export
bit_plus3: Bit -> Bit -> Bit -> Bit
bit_plus3 (MkBit b1) (MkBit b2) (MkBit cin) =
  MkBit (bool_xor (bool_xor b1 b2) cin)

||| Represents the carry value when adding three bits
export
bit_carry3: Bit -> Bit -> Bit -> Bit
bit_carry3 (MkBit b1) (MkBit b2) (MkBit cin) =
   MkBit (bool_or (bool_and (bool_xor b1 b2) cin) (bool_and b1 b2))

export
bit_if_then_else: (T: Type) -> Bool -> T -> T -> T
bit_if_then_else ty True tbranch _ = tbranch
bit_if_then_else ty False _ tbranch = tbranch

||| Returns True if two bits are equal and False otherwise.
export
bit_eq: Bit -> Bit -> Bool
bit_eq (MkBit b1) (MkBit b2) = bool_eq b1 b2

||| returns the negation or opposite of a bit
export
bit_not: Bit -> Bit
bit_not (MkBit b1) = (MkBit (bool_not b1))

||| returns the (MkBit True) if one of the given bits are 1
||| returns (MkBit False) if otherwise (both bits are 0)
export
bit_or: Bit -> Bit -> Bit
bit_or (MkBit b1) (MkBit b2) = (MkBit (bool_or b1 b2))

||| returns the bit equivalent to the boolean result of performing
||| bool_and on the booleans associated with the given bits.
export
bit_and: Bit -> Bit -> Bit
bit_and (MkBit b1) (MkBit b2) = (MkBit (bool_and b1 b2))

||| Creates the equivalent of bool_nand for bits
||| instead of returning a bool, it will return the equivalent boolean
export
bit_nand: Bit -> Bit -> Bit
bit_nand (MkBit b1) (MkBit b2) = (MkBit (bool_nand b1 b2))

||| creates the equivalent of bool_xor for bits
||| intead of returning a bool, it will return the equivalent boolean
export
bit_xor: Bit -> Bit -> Bit
bit_xor (MkBit b1) (MkBit b2) = (MkBit (bool_xor b1 b2))

export
implementation Eq Bit where
  eq b1 b2 = bit_eq b1 b2
  neq b1 b2 = bool_not (eq b1 b2)
