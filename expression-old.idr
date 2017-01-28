module expr

import nat
import variable
import state


public export
data Expr =
  LitExpr Nat |
  VarExpr Variable |
  PlusExpr Expr Expr |
  MinusExpr Expr Expr |
  MultExpr Expr Expr |
  ExpExpr Expr Expr |
  PredExpr Expr |
  SuccExpr Expr

exprEval: Expr -> State -> Nat
exprEval (LitExpr n) st = n
exprEval (VarExpr v) st = st v
exprEval (PlusExpr e1 e2) st =
  nat_plus   -- the result is the sum of
    (exprEval e1 st) -- the value of expr1
    (exprEval e2 st) -- and expr2
exprEval (MinusExpr e1 e2) st =
  nat_sub   -- the result is the subtraction of
    (exprEval e1 st) -- the value of expr1
    (exprEval e2 st) -- and expr2
exprEval (MultExpr e1 e2) st =
  nat_mult   -- the result is the multiplication of
    (exprEval e1 st) -- the value of expr1
    (exprEval e2 st) -- and expr2
exprEval (ExpExpr e1 e2) st =
  nat_pow
    (exprEval e1 st)
    (exprEval e2 st)
exprEval (PredExpr e1) st =
  nat_pred
    (exprEval e1 st)
exprEval (SuccExpr e1) st =
  nat_succ
    (exprEval e1 st)
