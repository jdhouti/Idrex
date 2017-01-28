module pair

import nat
import bool

||| A polymorphic pair type
public export
data Pair k v = MkPair k v

p1: Pair Nat Nat
p1 = MkPair nat_zero nat_zero

p2: Pair Bool Bool
p2 = MkPair True False

p3: Pair Bool Nat
p3 = MkPair True nat_one

||| Returnsk the first element in a pair
export
pair_first: Pair k v -> k
pair_first (MkPair k v) = k

||| Returns the second element in a pair
export
pair_second: Pair k v -> v
pair_second (MkPair k v) = v

||| Another way to write the pair_first function
||| Except this time, the constructor won't be visible
export
pair_key: Pair k v -> k
pair_key p = pair_first p

||| Another way to write the pay_second function
||| Except this time, the constructor won't be visible
export
pair_value: Pair k v -> v
pair_value p = pair_second p
