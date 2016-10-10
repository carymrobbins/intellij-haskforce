{-# LANGUAGE InstanceSigs #-}
module InstanceSigs00001 where

data T a = MkT a a
instance Eq a => Eq (T a) where
  (==) :: T a -> T a -> Bool   -- The signature
  (==) (MkT x1 x2) (MkTy y1 y2) = x1==y1 && x2==y2
