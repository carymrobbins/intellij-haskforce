{-# LANGUAGE GADTs #-}
module GADT where

data A a = A a

data GADT a b where
   B :: b -> b -> GADT A b

f :: GADT A a -> a
f (B _ end) = e<caret>nd