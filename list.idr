module list

import nat
import bool
import ifthenelse
import nat
import eq

public export
data List a =
  Nil |
  Cons a (List a)

||| Returns the a nat representing the lenght of a tail
||| For example given a list of (S (S Z)) -> nat_two
export
list_length: List a -> Nat
list_length Nil = nat_zero
list_length (Cons head tail) =
  nat_succ (list_length tail)

export
list_append: List a -> List a -> List a
list_append Nil l = l
list_append (Cons head tail) l =
  Cons (head) (list_append tail l)

export
list_head: List a -> (default: a) -> a
list_head Nil default = default
list_head (Cons h t) _ = h

export
list_tail: List a -> List a
list_tail Nil = Nil
list_tail (Cons h t) = t

export
list_sub: (index: Nat) -> List a -> (default: a) -> a
list_sub index Nil default = default
list_sub index (Cons head tail) default =
  ifthenelse
    (nat_eq index nat_zero)
    (head)
    (list_sub (nat_pred index) tail default)

{-
export
data ListNat =
  ||| Nil_at represens the empty list of natural numbers
  NilNat |
  ||| Con_nat head tail represents the
  ConsNat Nat ListNat
-}

||| Return a list starting with a given nat and descending by 1s to 0
||| Example: list_range 3 = (Cons 3 (Cons 2 (Cons 1 (Cons 0 Nil))))
||| Example: list_range 0 = (Cons 0 Nil)
||| Note: I've used shorthand for nats: E.g., 3 is really (S (S (S Z)))
export
list_range: Nat -> List Nat
list_range n =
  ifthenelse
    (nat_eq n nat_zero)
    (Cons nat_zero Nil)
    (Cons n (list_range (nat_pred n)))

||| Return a list starting with n decreasing by b until t incl
||| Example: list_range_by_to 11 3 2 = [11, 8, 5, 2]
||| Example: list_range_by_to 11 3 3 = [11, 8, 5]
||| Note: I've used shorthand for lists: [11, 5, 8] means (Cons 11 ... etc)
{-
list_range_by_to: (n: Nat) -> (b: Nat) -> (t: Nat) -> List Nat
list_range_by_to n b t =
  ifthenelse (nat_eq n t)
    (Cons n Nil)
    (ifthenelse
      (nat_lt n t)
      (Nil)
      (Cons n (list_range_by_to(nat_sub n b) b t)))
-}
export
list_sum: List Nat -> Nat
list_sum Nil = nat_zero
list_sum (Cons h t) =
  nat_plus
    (h)
    (list_sum t)

||| Given a list of nats, return their product
export
list_prod: List Nat -> Nat
list_prod Nil = nat_one
list_prod (Cons h t) =
  nat_mult (h) (list_prod t)

--- CORRECTED HOMEWORK STARTS HERE.

--- HOMEWORK STARTS HERE.

||| The function (list_range_by_to n b t) should teturn a list of natural
||| numbers starting with n, decreasing by b, until t is reached or passed,
||| where t is included in the result if it is reached exactly.
||| Examples:
|||   list_range_by_to 5 1 1 = [5, 4, 3, 2, 1]
|||   list_range_by_to 5 2 1 = [5, 3, 1]
|||   list_range_by_to 5 3 1 = [5, 2]
||| To avoid infinite loops, we specify if b = 0, the result is Nil
||| Example:
|||   list_range_by_to 5 0 1 = Nil
||| To be clear, if n = t the result is [n] (a list with just n)
||| Example:
|||   list_range_by_to 5 2 5 = [5] ("from 5 down to 5 by twos is just [5]")
||| Tere is one case for which this specification is ambiguous. Identify is
||| and represent it in the definition of r6, below. For that case, the
||| result should be Nil.
export
list_range_by_to: (n: Nat) -> (b: Nat) -> (t: Nat) -> List Nat
list_range_by_to n b t =
  ifthenelse (nat_eq n t)
    (Cons n Nil)
    (ifthenelse
      (nat_lt n t)
      (Nil)
      (Cons n (list_range_by_to(nat_sub n b) b t)))

||| Given a list of natural numbers return the sum of their squares
||| Example: list_sum_squares Nil = nat_zero
||| Example: list_sum_squares [0] = 0
||| Example: list_sum_squares [3, 4, 1, 2] = 30
export
list_sum_squares: List Nat -> Nat
list_sum_squares Nil = nat_zero
list_sum_squares (Cons h t) =
  nat_plus
    (nat_mult h h)
    (list_sum_squares t)

||| Given a list of natural numbers, return a list of Boolean values
||| where a Boolean is True if the corresponding number is even and
||| False otherwise.
||| Example: list_nat_ev_bool [5,4,3,2,1,0] = [F,T,F,T,F,T]
export
list_nat_ev_bool: List Nat -> List Bool
list_nat_ev_bool Nil = Nil
list_nat_ev_bool (Cons h t) =
  Cons
    (nat_even h)
    (list_nat_ev_bool t)

||| Given a list of natural numbers, return the sublist of even numbers
||| Example: list_filter_even [5, 4, 3, 2, 1, 4, 2, 0] = [4, 2, 4, 2, 0]
export
list_filter_even: List Nat -> List Nat
list_filter_even Nil = Nil
list_filter_even (Cons h t) =
  ifthenelse
    (nat_even h)
    (Cons h (list_filter_even t))
    (list_filter_even t)

||| Given a list of Booleans, return the sublist of True ones
||| Example: list_filter_even [False, False] = Nil
||| Example: list_filter_even [T, F, T, F] = [T, T]
export
list_filter_True: List Bool -> List Bool
list_filter_True Nil = Nil
list_filter_True (Cons h t) =
  ifthenelse
    (bool_id h)
    (Cons h (list_filter_True t))
    (list_filter_True t)

-- HOMEWORK ENDS HERE

||| Give it a list of a nats and return a list of those nats incremented by one.
||| For example if given a list of [1,2,3] it would return [2,3,4]
export
list_inc: List Nat -> List Nat
list_inc Nil = Nil
list_inc (Cons h t) =
  Cons
    (nat_succ h)
    (list_inc t)

||| Give it a list of nats and return a list of all of those nats squared.
||| For example if give a lsit of [1,2,3] it would return [1,4,9].
export
list_map_square: List Nat -> List Nat
list_map_square Nil = Nil
list_map_square (Cons h t) =
  Cons
    (nat_mult h h)
    (list_map_square t)

||| A higher order function that takes a function and a list
||| and applies that function to every element in that list using recursion.
||| For example if given list_map_square and list [1,2,3]
||| It would return [1,4,9]
export
list_map: (fn: a -> b) -> List a -> List b
list_map func Nil = Nil
list_map func (Cons h t) =
  Cons
    (func h)
    (list_map func t)

||| A higher order function that takes a function, an id and a list.
||| The id is the last list Nil in order to not cancel out the entire recursion
||| or return an inaccurate value when using recursion.
||| For example if given list_map_square, the id would have to be nat_one
||| because when reaching the Nil list, multiplying by 0 would cancel everything out.
export
list_foldr: (fn: a -> a -> a) -> (id: a) -> (l: List a) -> a
list_foldr fn id Nil = id
list_foldr fn id (Cons h t) =
  fn h (list_foldr fn id t)

||| Return the sublist of elements for which a predicate is true
||| For example if given the nat_even function and a list of [2, 4, 5]
||| list_filter will return a list of nats -> [2, 4]
export
list_filter: (fn: a -> Bool) -> List a -> List a
list_filter predicate Nil = Nil
list_filter predicate (Cons head tail) =
  ifthenelse
    (predicate head)
    (Cons head (list_filter predicate tail))
    (list_filter predicate tail)

||| Uses the Eq implementation to check if two lists of either Bool or Nat
||| are identical.
||| If given [True, False, True] and [False, True, False] -> returns bool False
public export
list_eq: Eq a => List a -> List a -> Bool
list_eq Nil Nil = True
list_eq Nil (Cons h t) = False
list_eq (Cons h t) Nil = False
list_eq (Cons h1 t1) (Cons h2 t2) =
  bool_and
    (eq h1 h2)
    (list_eq t1 t2)

||| Returns a list of just nat_ones regardless of what kind of list is given.
||| For example, if given [True, True, False] it would return [1, 1, 1].
||| This could be used to identify how many elements are in a list by adding all
||| of the ones after the given list has been mutated.
export
const_one: List a -> List Nat
const_one Nil = Nil
const_one (Cons h t) =
  Cons
    (nat_one)
    (const_one t)

||| Returns the number of elements in a list using the const_one function.
||| For example if given [5, 6, 7] -> Const_one -> [1, 1, 1] -> list_length' -> 3
export
list_length': List a -> Nat
list_length' (Cons h t) =
  list_foldr
    (nat_plus)
    (nat_zero)
    (const_one (Cons h t))

||| Returns true if all nats in a list are even
||| Given a list of [2, 3, 4] -> return false because 3 is not even
export
allEven: List Nat -> Bool
allEven (Cons h t) =
  list_foldr
    (bool_and)      -- given function to compare the id to the list
    (True)          -- this is the id so that the whole function isn't false
    (list_map       -- returns the list of booleans based on even values
      (nat_even)
      (Cons h t))

||| Returns true if at least one nat in a given list is even
||| Given a list of [3, 4, 5] -> returns true because 4 is even
export
someEven: List Nat -> Bool
someEven (Cons h t) =
  list_foldr
    (bool_or)       -- given function to compare the id to the list
    (False)         -- this could either be true or False
    (list_map       -- returns the list of booleans based on even values
      (nat_even)
      (Cons h t))

export
numEven: List Nat -> Nat
numEven (Cons h t) =
  list_length'
    (list_filter
      (bool_id)
        (list_map
          (nat_even)
          (Cons h t)))
