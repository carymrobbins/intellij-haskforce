{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE UnboxedTuples #-}
module Kind00001 where

type Pr = (# Int, Int #)

type T (f :: * -> *) = f Int

data S

data a :*: b = Foo a b

x :: Int :*: Bool
x = Foo
