module option

import bool
import nat

public export
data Option a = None | Some a

o1: Option Nat
o1 = None

o2: Option Nat
o2 = Some nat_one

bo: Option Bool
bo = None

bo': Option Bool
bo' = Some True

export
isNone: Option a -> Bool
isNone None = True
isNone _ = False
