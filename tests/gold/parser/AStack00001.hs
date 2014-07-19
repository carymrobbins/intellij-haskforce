module AStack00001( Stack, push, pop, top, size ) where
data Stack a = Empty  
             | MkStack a (Stack a)  
 
push :: a -> Stack a -> Stack a  
push x s = MkStack x s  
 
size :: Stack a -> Int  
size s = length (stkToLst s)  where  
           stkToLst  Empty         = []  
           stkToLst (MkStack x s)  = x:xs where xs = stkToLst s  
 
pop :: Stack a -> (a, Stack a)  
pop (MkStack x s)  
  = (x, case s of r -> i r where i x = x) -- (pop Empty) is an error  
 
top :: Stack a -> a  
top (MkStack x s) = x                     -- (top Empty) is an error
