module fib

import public nat
import ifthenelse

-- this module does not need a data type! :)

||| part b of question 3
fib: Nat -> Nat
fib n =
  ifthenelse
    (nat_eq n nat_zero)                   -- if n = 0
    (nat_zero)                            -- answer is 0
    (ifthenelse
      (nat_eq n nat_one)                  -- if n = 1
      (nat_one)                           -- answer is 1
      (nat_plus
        (fib (nat_pred n))                -- else, do this
        (fib (nat_pred (nat_pred n)))))
