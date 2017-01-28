module icon

import public bit
import public nat
import ifthenelse
import byte4
import public byte8

||| Creates the icon data type using 8 byte8s
data Icon = MkIcon Byte8 Byte8 Byte8 Byte8 Byte8 Byte8 Byte8 Byte8

||| part b of question 2
||| here we create the variable gamby of type Icon
gamby: Icon
gamby =
  MkIcon
    (byte8_new B1 B1 B0 B0 B0 B0 B1 B1)       -- first line
    (byte8_new B1 B0 B1 B1 B1 B1 B0 B1)       -- second line
    (byte8_new B0 B1 B0 B1 B0 B1 B1 B0)       -- etc.
    (byte8_new B0 B1 B0 B1 B0 B1 B1 B0)
    (byte8_new B0 B1 B1 B1 B1 B0 B1 B0)
    (byte8_new B0 B1 B0 B0 B0 B1 B1 B0)
    (byte8_new B1 B0 B1 B1 B1 B1 B0 B1)
    (byte8_new B1 B1 B0 B0 B0 B0 B1 B1)

-- note: I cannot use MkByte8 about because the constructor is hidden
-- and reserved for the byte8.idr file only

||| part c of question 2
row: Nat -> Icon -> Byte8
row index (MkIcon a b c d e f g h) =
  ifthenelse
    (nat_eq index nat_zero)     -- if index is 0
    (a)                         -- return the first byte8
    (ifthenelse
      (nat_eq index nat_one)    -- if index is 1
      (b)                       -- return the second byte8
      (ifthenelse
        (nat_eq index nat_two)  -- if index is 2
        (c)                     -- return the third byte8
        (ifthenelse
          (nat_eq index nat_three)      -- if index is 3
          (d)                           -- return the fourth byte8
          (ifthenelse
            (nat_eq index nat_four)     -- if index is 4
            (e)                         -- return the fifth byte8
            (ifthenelse
              (nat_eq index nat_five)   -- if index is 5
              (f)                       -- return the sixth byte8
              (ifthenelse
                (nat_eq index nat_six)        -- if index is 6
                (g)                           -- return the seventh byte8
                (ifthenelse
                  (nat_eq index nat_seven)    -- if index is 7
                  (f)                         -- return the eighth byte8
                  (a))))))))                  -- if all else fails, return the first byte8

-- note: in the instructions, it mentioned that if the given index is greater than 7
-- I should return the "0 byte." I assumed the "0 byte" was the Byte8 on line 0 and not B0
-- explaining why I return a on line 55

||| part d of question 2
mask: Icon -> Icon -> Icon
mask
  (MkIcon a b c d e f g h)       -- every letter represents a byte8
  (MkIcon i j k l m n o p) =
    (MkIcon
      (byte8_and a i)     -- computes the "and" byte8 of both corresponding bytes8
      (byte8_and b j)     -- here as well
      (byte8_and c k)     -- etc.
      (byte8_and d l)
      (byte8_and e m)
      (byte8_and f n)
      (byte8_and g o)
      (byte8_and h p))

-- note: the above function uses the byte8_and function created in byte8.idr
-- on lines 212-224.

||| part e of question 2
||| creation of the top half icon
topHalf: Icon
topHalf =
  MkIcon
    (byte8_new B1 B1 B1 B1 B1 B1 B1 B1)   -- beginning of top half
    (byte8_new B1 B1 B1 B1 B1 B1 B1 B1)
    (byte8_new B1 B1 B1 B1 B1 B1 B1 B1)
    (byte8_new B1 B1 B1 B1 B1 B1 B1 B1)
    (byte8_new B0 B0 B0 B0 B0 B0 B0 B0)   -- beginning of second half
    (byte8_new B0 B0 B0 B0 B0 B0 B0 B0)
    (byte8_new B0 B0 B0 B0 B0 B0 B0 B0)
    (byte8_new B0 B0 B0 B0 B0 B0 B0 B0)

||| create the maskedResult of both gamby and topHalf
maskedResult: Icon
maskedResult =
  mask          -- apply the mask function defined above
    (gamby)     -- to gamby
    (topHalf)   -- and topHalf

||| part f of question 2 (extra credit)
||| uses the byte8_or function defined in byte8.idr on lines 231-243
doubleExposure: Icon -> Icon -> Icon
doubleExposure
  (MkIcon a b c d e f g h)      -- the first icon it needs
  (MkIcon i j k l m n o p) =    -- the second icon it needs
    (MkIcon
      (byte8_or a i)     -- computes the "or" byte8 of both corresponding bytes8
      (byte8_or b j)     -- here as well
      (byte8_or c k)     -- etc.
      (byte8_or d l)
      (byte8_or e m)
      (byte8_or f n)
      (byte8_or g o)
      (byte8_or h p))

||| here is proof that it works by applying it on both topHalf and gamby :)
myproof: Icon
myproof =
  doubleExposure
    (gamby)       -- uses gamby as one of the icons
    (topHalf)     -- uses topHalf as the other icon

||| more proof?
moreProof: Icon
moreProof =
  doubleExposure
    (gamby)
    (maskedResult)
