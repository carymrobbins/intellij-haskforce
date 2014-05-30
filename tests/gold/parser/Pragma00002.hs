module Pragma00002 where

data Vector a = Pull DIM1  {-# UNPACK #-} !a

f x = ({-# CORE "foo" #-} show) ({-# CORE "bar" #-} x)

g x = let xs = {-# SCC "X" #-} [1..1000000] in x