{-# OPTIONS_GHC -fglasgow-exts #-}
{-# ANN module (Just "A `Maybe String' annotation") #-}
module Pragma00001 where

{-# ANN type Foo (Just "A `Maybe String' annotation") #-}
data Foo = Push DIM1

type Vector a = Pull DIM1 a
{-# DEPRECATED Vector "Use Pull instead" #-}

unsafeVector = id
{-# WARNING unsafeVector "This is unsafe; I hope you know what you're doing" #-}
{-# INLINE unsafeVector #-}

safeVector = id
{-# NOINLINE [2]  safeVector #-}

safeVector2 = id
{-# NOINLINE [~2] safeVector2 #-}

f :: Eq a => a -> b -> b
f 0 x = x
f a x = f (a - 1) x
{-# SPECIALISE f :: Int -> b -> b #-}

-- FIXME: Waiting for https://github.com/haskell-suite/haskell-src-exts/pull/112
g x = x
{-# INLINE_CONLIKE [1] g #-}

h :: a -> a
h x = x
{-# SPECIALISE INLINE [~2] h :: Int -> Int #-}

i x = x
{-# ANN i (Just "Hello") #-}

map2 = map
{-# RULES
"map2/map2"    forall f g xs.  map2 f (map2 g xs) = map2 (f.g) xs
 #-}
