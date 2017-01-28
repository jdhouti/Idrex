module nat

import public bool
import public eq

export
data Nat =
  ||| Z constructs a representation of zero.
  Z |
  ||| S n constructs a reprensation of the successor of n.
  S Nat

||| This nat just represents zero
export
nat_zero: Nat
nat_zero = Z

||| This nat represents one
export
nat_one: Nat
nat_one = S Z

||| This nat represents two
export
nat_two: Nat
nat_two = S nat_one

export
nat_three: Nat
nat_three = S nat_two

export
nat_four: Nat
nat_four = S nat_three

export
nat_five: Nat
nat_five = S nat_four

export
nat_six: Nat
nat_six = S nat_five

export
nat_seven: Nat
nat_seven = S nat_six

export
nat_eight: Nat
nat_eight = S nat_seven

export
nat_nine: Nat
nat_nine = S nat_eight

export
nat_ten: Nat
nat_ten = S nat_nine

||| Represents the given nat.
export
nat_id: Nat -> Nat
nat_id n = n

||| Represents the number that comes after n
export
nat_succ: Nat -> Nat
nat_succ n = S n

||| Represents the number that comes before n
||| It does this by first evaluating the number that comes after n
||| And then returning the number that comes before it.
export
nat_pred: Nat -> Nat
nat_pred Z = Z
nat_pred (S n) = n

||| Adds two different nats
export
nat_plus: Nat -> Nat -> Nat
nat_plus Z n = n
nat_plus (S n) m = S (nat_plus n m)

||| Subtracts two nats from eachother.
export
nat_sub: Nat -> Nat -> Nat
nat_sub Z n = Z
nat_sub n Z = n
nat_sub (S n) (S m) = (nat_sub n m)

||| Determines whether a nat is even or not.
export
nat_even: Nat -> Bool
nat_even Z = True
nat_even (S Z) = False
nat_even (S (S n)) = nat_even (n)

||| Determins whether two given nats are equals to eachother.
export
nat_eq: Nat -> Nat -> Bool
nat_eq Z Z = True
nat_eq Z (S n) = False
nat_eq (S n) Z = False
nat_eq (S m) (S n) = nat_eq m n

||| Compute the product of two nats
export
nat_mult: Nat -> Nat -> Nat
nat_mult Z m = Z
nat_mult (S n) m = nat_plus m (nat_mult n m)

-- LAB PROBLEMS: Fill in the holes and/or write code as
-- indicated in the following five programming problems.

-- PROBLEM #1

||| Return true if the first natural number argument
||| is less than or equal to the second argument
export
nat_le: Nat -> Nat -> Bool
nat_le Z Z = True
nat_le Z (S n) = True
nat_le (S n) Z = False
nat_le (S n) (S m) = (nat_le n m)

-- PROBLEM #2

||| Return true if the first natural number, n, is
||| strictly less than the second, m (i.e., n < m).
-- Hint: n < m if n <= m and n is not equal to m.
-- Use Boolean algebra!
export
nat_lt: Nat -> Nat -> Bool
nat_lt n m =
  (nat_le (S(S n)) (S m))

-- PROBLEM #3

-- Implement a function, nat_gt n m, that returns true
-- if n > m, and otherwise false (where n and m are Nats).
-- Hint: Use Boolean algebra to combine results from several
-- functions you already have.
export
nat_gt: Nat -> Nat -> Bool
nat_gt Z Z = False
nat_gt Z (S m) = False
nat_gt (S n) Z = True
nat_gt (S n) (S m) = (nat_gt n m)

-- PROBLEM #4

-- Implement a function, nat_ge n m, that returns true if
-- n is greater than or equal to m, and false otherwise.
export
nat_ge: Nat -> Nat -> Bool
nat_ge Z Z = True
nat_ge Z (S m) = False
nat_ge (S n) Z = True
nat_ge (S n) (S m) = (nat_ge n m)

-- PROBLEM #5

||| Return n raised to the power m. Recall that any
||| natural number to the zero power is 1, and that
||| a natural number, n, to the m'th power (n > 0) is
||| n times n to the (m-1) power. You will have to
||| replace the single rule in the following code
||| with rules for base and recursive cases.
export
nat_pow: Nat -> Nat -> Nat
nat_pow Z Z = Z
nat_pow (S n) Z = nat_one
nat_pow Z (S m) = Z
nat_pow n (S m) = (nat_mult n (nat_pow n m))

||| Returns the factorial of a given number.
||| For example if given 3, it will return:
||| 3 * 2 * 1 = 6
export
nat_fac: Nat -> Nat
nat_fac Z = (S Z)
nat_fac (S n) =
  nat_mult
    (S n)
    (nat_fac n)

||| Returns the square of a given integer
||| For example if give 3, it will return:
||| 3 * 3 = 9
export
nat_square: Nat -> Nat
nat_square Z = Z
nat_square (S n) = nat_mult (S n) (S n)

||| Returns the result of a given function applied to a Nat
||| For example if nat_square is given with a nat of 3, it should return:
||| nat_square 3 (which is 3 * 3) -> nat_nine
export
apply1: (fn: Nat -> Nat) -> Nat -> Nat
apply1 func n =
  func n

||| Returns the result of a function that takes 2 Nats applied to 2 Nats
||| For example if nat_mult is given Nat of 3 and 2, it should return:
||| nat_mult nat_two nat_three (2 * 3) -> nat_six
export
apply2: (fn: Nat -> Nat -> Nat) -> Nat -> Nat -> Nat
apply2 func n m =
  func n m

||| Returns the result of a function applied to the result of another function
||| that takes a Nat.
||| For example if nat_succ, nat_square and 3 is given, it should return:
||| nat_succ (nat_square nat_three) ((3 * 3) + 1) -> nat_ten
export
apply3: (fn: Nat -> Nat) -> (fn: Nat -> Nat) -> Nat -> Nat
apply3 func1 func2 n =
  func1
    (func2 n)

export
implementation Eq Nat where
  eq b1 b2 = nat_eq b1 b2
  neq b1 b2 = bool_not (eq b1 b2)
