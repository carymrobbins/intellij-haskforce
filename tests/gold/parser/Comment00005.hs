module Comment00005 where

z = 2

{-
This function implements double buffering of state for imnmutable
seqential loops (forLoop and whileLoop).  The intention is to generate
the minimal amount of copying (especially deep array copies) while
also avoiding frequent references to data that can not be allocated in
registers.

The state of the loop is implemented by two variables so that the
loop body reads from one and writes to the other. At the end of the
body, the contents of the second (write) variable is shallow copied to
the first (read) variable. In order to avoid inadvertent sharing of
data referenced by pointers in the variables (array buffers, for
instance), the pointers in the state are swapped rather than just
copied so that the end up in the variable written to. Finally the read
variable is shallow copied to the result location.

There are some simplifying cases:
    - When the target lvalue loc is a variable, and thus cheap to
      access, it is reused as the read state variable
    - When the type of the state is scalar, so that assignment is
      atomic, only one state variable is used and it is both read and
      written to in the loop body, eliminating the shallow copy in the
      loop body.
The strategy implemented a com promise between different design constraints:
    - Avoid deep copying of arrays (that is what the double buffering is for)
    - Avoid shallow copying if possible
    - Avoid memory leaks of arrays and ivars
-}

f :: Int -> Int
f x = y

