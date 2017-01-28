{-
Drop this file into your mylang directory. Follow the instructions.
Incorporate your answers into this file. Submit the resulting file
through Collab. BE SURE TO **SAVE YOUR FILE** BEFORE YOU SUBMIT IT!

We recommend that you quickly skim this test before you start except
for the extra credit question at the end. Do not consider the extra
credit question unless and until you've finished the rest of the test.

The test has 19 questions, each worth 5 points, except for the last,
which is a 10-point question, and the only one that uses Python. (The
extra credit question follows the last regular question on the exam.)

This is an open notes exam. The collaboration policy is that you may
not communicate with *anyone other than the instructor about this exam
until all students have completed the exam*. The honor code is strictly
enforced. DO NOT EMAIL, CHAT, TEXT, LOOK AT, ALLOW ANOTHER TO LOOK AT,
or collaborate in any other way with anyone on or about this exam.
-}

module final_exam

import expression
import state
import variable
import nat
import command
import pair
import dictionary
import list

-- Complete the following statements

{-
1 [5 pts]. Evaluating an expression in a given state yields (returns,
reduces to a) "value" Hint: the answer is one word, five letters
long, starting with 'v'.
-}


{-
2 [5 pts]. Evaluating a command in a given state yields a "state"
-}


{-
3 [5 pts]. Define e1 to be an expression in our little language
equivalent to the Python expression "X + 1". Answer by replacing
the hole in the following partial program with your answer).
-}

X: Variable Nat
X = variable_new nat_one

e1: NatExpr
e1 =
  NatPlusExpr
    (NatVarExpr X)
    (NatLitExpr nat_one)

{-
4 [5 pts]. write code in our little language to assign to n1 the
result of evaluating e1 (from the previous question) in the initial
state (state_init). Fill in the holes for both the type and value
of n1. What value do you expect n1 to have (answer in the comment
after the code)? Note: you can use the REPL to check your answer.

-}

n1: State
n1 = CommandEval e1 st
-- expect <nat_two or 2>


-- The following code defines st as a shorthand for init_state.
-- the intial state is 0
st: State
st = state_init


{-
5a [5 pts]. Define c1 to be a command in our little language
equivalent to X = 5 in Python. (Fill in the hole in the code.
We define the variable, X, for you, and declare c1 to be of
type Command.
-}

c1: Command
c1 =
  NatAssign
    X
    (NatLitExpr nat_five)
    -- if nat_five is not defined --> nat_succ(nat_succ(nat_three)) could be used instead

{-
5b. What value is assigned to X by init_state (st)? <nat_zero>
-}


{-
6 [ 5 pts]. Define st' to be the state that results from the
evaluation of c1 in the initial state, st. Fill in the hole in this
code with the right Idris expression to evaluated c1 in state st.
-}

st': State
st' = CommandEval c1 st


-- 7 [5 pts]. Define c2 to be a command equivalent to X = X + 1 in Python

c2: Command
c2 =
  NatAssign
    (NatPlusExpr
      (NatVarExpr X)
      (NatLitExpr nat_one))


{-
8 [5 pts]. Define st'' to be the state that results from the
evaluation of c2 in state st' (which is itself the result of
evaluating c1 in the state, st.) Fill in the holes accordingly.
-}

st'': State
st'' = CommandEval c2 st'


{-
9 [5 pts]. Define c3 to be the command that is the sequential
composition of c1 and c2.
-}

-- composition is NOT addition.
-- composition is simply grouping

c3: Command
c3 = Seq c1 c2

{-
10 [5 pts]. What is the value of X in the state that results from
evaluating c3 (the sequential composition of commands) in the initial
state?
DO NOT COMBINE THEM
Your answer: nat_five nat_six


-}


{-
11 [5 pts]. Explain in precise English the rule for evaluating a
command, c3, in a given state, st, where c3 is itself a sequential
composition of two smaller commands, c1 and c2 (as above). Hint:
Read and explain the relevant part of the definition of the command
evaluation function.

Answer here: Since the sequential function takes two commands, in order to evaluate
the state of the sequential function, the state of the two commands need to be evaluated
first. In order to do so, the state of the command function needs to be evaluated for the
appropriate parts such as NatAssign or BoolAssign for example.

Answer: Since c3 is a command, we will use CommandEval and since CommandEval takes
both a command and a state, it will evaluate that command in that state and return that state
it will then use that returned state to evaluate the second command to that state.



-}


{-
12 [5 pts]. Explain in precise English the rule for evaluating
a While command with a condition, c, and a body (command), b, in
a state, st. Hint: There are two cases to consider. Be careful
to express your answer in precise terms. Be sure you explain how
you obtain the desired final state starting with the given state,
st. Hint: Again, go read the definition before trying to answer.

Answer here: In order to evaluate the state of a while command, the state of both
the conidtion and the body/command needs to be evaluated.



-}


{-
The next few questions are about dictionaries. One way to think about
a dictionary is that it represents a"table" with two columns. In the
first column are values of a "key" type. In the second column are
values of a corresponding "value" type. For example, a telephone book
is a kind of table, with persons' names in the first column (keys) and
corresponding telephone numbers in the second column.
-}


{-
13 [5 pts]. Define d to be an empty dictionary in which keys are
of type Nat and values are of type Bool. Hint: You probably don't
remember all the details about how to create dictionaries. Go read
the definition of the Dictionary type. As usual, fill in the holes.
-}

-- in this case, I should've used dictionary_new
-- the definition of d indicates that it's keys are nats and values are bool
d: Dictionary Nat Bool
d = dictionary_new Nil -- the dictionary is empty


{-
14 [5 pts]. Define d' to be a dictionary obtained by inserting the
key-value pair, (1, True) into d. Hint: Read the definition of the
insert operation carefully. You'll have to think about the types of
the arguments to insert. Make sure you're providing the right types
of arguments here.
-}

d': Dictionary k v
d' = dictionary_insert d (MkPair nat_one True) -- you can only insert pairs into dictionaries.


{-
15 [5 pts]. What is the *type* of the return value of dictionary_lookup
applied to d' and some key value?

<A boolean or whatever type is paired with the given type.>

Explain in plain English why this type is required here:

<It's required in order for the dictionary to return the paired type.
This is to find which value or type is paired with the given type.>

-}


{-
16 [5 pts]. Suppose you have a list of natural numbers and you want
a new list that's just like the given list except that each number is
increased by one. So, for example, if you're given the list [1,2,3]
you would get back the list [2,3,4]. Complete the following code to
carry out this operation using one of your higher-order functions for
lists. Hint: Remember that the successor function, nat_succ, increases
a given Nat by one. Note that you can use the REPL to check that your
code works.
-}

l: List Nat
l = Cons nat_one (Cons nat_two (Cons nat_three Nil))

-- use the list_map which takes a function and a list and applies that function
-- to every element in that list.
l': List Nat
l' = list_map nat_succ l



{-
17 [5 pts]. Write a data type definition for a type called Foo,
where each value of type Foo is a tuple with three values (parts,
fields): a Nat, a Bool, and a List of Nat. (Foo is just a made-up
name.) Call the constructor MkFoo.

<data Foo n b l = MkFoo Nat Bool (List a)>

Now, define f to be a value of this type. We don't care what
values you provide for the Nat, Bool, and list components of f;
just show us that you can create a value of this type.

<f: Foo
f = MkFoo nat_one True Nil>


-}


{-
18 [5 pts]. Write a projection function called getNat that takes
a value of type Foo and that returns the value of its Nat field.
Hint: Destructure the value of type Foo to get at its parts. Then
write code that declares q to be of type Nat and that binds q to
the result of applying getNat to f (from the previous question). Use
the REPL to confirm that your code works.


<getNat: Foo -> Nat
getNat MkFoo n b l = n

q: Nat
q = getNat>
-}





{-
19 [10 pts]. Write a Python program that does the following:
(1) get a number as input from the user, assigned to the variable X
(2) get a number as input from the user, assigned to the variable Y
(3) assign to the variable Z the sum of X and Y
(4) assign to the variable W the product of X and Y
(5) assign to the variable Q the quotient of Z divided by W
(6) print (output to the user) the value of Q

HHint: Recall that the "input()" procedure returns a string. You
need to convert the String value to an "int" before you can use
numeric functions such as plus and times on the input value.

Copy and paste your answer here (inside this comment). You may
use repl.it or PyCharm to write and check your code if you wish.

<
x = int(input())
y = int(input())
z = x + y
w = x * y
q = z % w

print(q)
>

-}

{-
Extra credit: Consider the imperative program we wrote to add
up the numbers from 1 to n. The core of the program is a While
loop. At the start of the loop, X is assigned the value of the
number, n, up to which we want to compute the sum, and Y, which
we use to accumulate the partial sum as we iterate the loop, is
assigned the value, zero. Make a 3-column table here showing the
values of X, of Y, and the desired answer (which, when n = 5 is
15, for example) right before the loop starts, and at the end of
each iteration of the loop. Note that the values of X and Y will
change in each row, but the final column will always be the same.
Write a mathematical expression that relates X, Y, and the desired
answer at all points during the computation. (We call an expression
of this kind a "loop invariant" by the way.)
-}
