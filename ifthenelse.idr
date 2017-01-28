module ifthenelse

import public bool

export
ifthenelse: {t: Type} -> Bool -> t -> t -> t
ifthenelse True tbranch _ = tbranch
ifthenelse False _ fbranch = fbranch
