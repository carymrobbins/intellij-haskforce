module Minimal00001 where

class HasFoo a where
    {-# MINIMAL foo #-}
    foo :: a -> String
    bar :: a -> String

class HasFoo' a where
    foo' :: a -> String
    {-# MINIMAL foo' #-}
    bar' :: a -> String

class HasFoo'' a where
    foo'' :: a -> String
    bar'' :: a -> String
    {-# MINIMAL foo'' #-}
