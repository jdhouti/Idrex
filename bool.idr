{-
  Julien Dhouti
  Computing Id: jd2wm
-}

module bool

||| A data type the values of which represent Boolean true and false
public export -- This allows for True and False to be seen to the user
data Bool =
  ||| Term representing Boolean true
  True |
  ||| Term representing Boolean false
  False

-- Begin the definition of uninary functions

||| Representation of identity function on Boolean values
export
bool_id: Bool -> Bool
bool_id b = b

||| Representation of negation on Boolean values
export
bool_not: Bool -> Bool
bool_not True = False
bool_not False = True

||| Representation of forced positive on Boolean values
export
bool_true: Bool -> Bool
bool_true _ = True

||| Representation of negation on all Boolean values
export
bool_false: Bool -> Bool
bool_false _ = False

-- Begin the defintion of binary functions

||| Representation of affirmation when given two True booleans
export
bool_and: Bool -> Bool -> Bool
bool_and True True = True
bool_and _ _ = False

||| Representation of negation when given two False booleans
export
bool_or: Bool -> Bool -> Bool
bool_or False False = False
bool_or _ _ = True

||| Representation of negation when bool_and returns true
export
bool_nand: Bool -> Bool -> Bool
bool_nand b1 b2 = bool_not(bool_and b1 b2)

||| Representation of negation when two given booleans are different
export
bool_implies: Bool -> Bool -> Bool
bool_implies True False = False
bool_implies _ _ = True

-- Similar to bit arithmatic

||| Representation of negation when given two identical booleans
export
bool_xor: Bool -> Bool -> Bool
bool_xor False False = False
bool_xor True True = False
bool_xor _ _ = True

||| Representation of affirmation when givent two idential booleans
export
bool_equiv: Bool -> Bool -> Bool
bool_equiv True True = True
bool_equiv False False = True
bool_equiv _ _ = False

||| Boolean equality
export
bool_eq: Bool -> Bool -> Bool
bool_eq b1 b2 = bool_equiv b1 b2

||| Representation of affirmation when first given boolean is True
export
bool_if_then_else: Bool -> Bool -> Bool -> Bool
bool_if_then_else True t _ =  t
bool_if_then_else False _ f = f
