module facTest

import command
import nat
import variableTest   -- defines variables X, Y, Z
import variable
import expression
import state

{-
How to write the following code in python:
(There are numerous ways of writing this.)
x = 2 	# this can be any number
y = 0	  # this can be any number
z = x

if y == 0:
	x = 1

else:
	if y == 1:
		x = x

	else:
		while y != 1:
			x = z * x
			y -= 1
-}

-- X = 2
XGetsTwo: Command
XGetsTwo =
  NatAssign
    X
    (NatLitExpr nat_two)

-- Y = 0
YGets0: Command
YGets0 =
  NatAssign
    Y
    (NatLitExpr nat_zero)

-- U = X      here Z is replaced with U (in comparison to the python script)
U: Command
U = NatAssign U (NatVarExpr X)

-- Y == 0 (True or False?)
ChkYis0: BoolExpr
ChkYis0 =
  BoolEqExpr
    (NatValExpr Y)
    (NatLitExpr nat_zero)

-- Y == 1
Yis1: BoolExpr
Yis1 =
  BoolEqExpr
    (NatValExpr Y)
    (NatLitExpr nat_one)

-- Y != 1
YNot1: BoolExpr
YNot1 =
  BoolNeqExpr
    (NatValExpr Y)
    (NatLitExpr nat_one)

-- X = 1
XGetsOne: Command
XGetsOne =
  NatAssign
    X
    (NatLitExpr nat_one)

-- X = X * U
multXandU: Command
multXandU =
  NatAssign
    X
    (NatMultExpr (NatVarExpr X) (NatVarExpr U))

-- Y = Y - 1 or Y -= 1
decrY: Command
decrY =
  NatAssign
    Y
    (NatMinusExpr (NatVarExpr Y) (NatLitExpr nat_one))

myFac: Command
myFac =
    Seq
    (NatAssign X (NatLitExpr nat_two))   -- x = 2
    (Seq
        (NatAssign U (NatVarExpr X))  -- z = x
        (Seq
          (NatAssign Y (NatLitExpr nat_three))) -- y = 3
          (IfThenElse
            (BoolEqExpr (NatValExpr Y) (NatLitExpr nat_zero))
            (NatAssign X (NatLitExpr nat_one))
            (IfThenElse
              (BoolEqExpr (NatValExpr Y) (NatLitExpr nat_one))
              (NatAssin X (NatVarExpr X))
              (While
                (BoolNeqExpr (NatValExpr Y) (NatLitExpr nat_one))
                (Seq
                  (NatAssign X (NatMultExpr (NatVarExpr X) (NatVarExpr U))
                  (Seq
                    (NatAssign Y (NatMinusExpr (NatVarExpr Y) (NatLitExpr nat_one))))))))))
