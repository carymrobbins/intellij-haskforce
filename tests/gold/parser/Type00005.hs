{-# LANGUAGE DataKinds, TypeFamilies #-}

module Main where

type family F a where
  F Int  = Bool
  F Bool = Char
  F a    = Bool

type family And (a :: Bool) (b :: Bool) :: Bool where
  And False c     = False
  And True  d     = d
  And e     False = False
  And f     True  = f
  And g     g     = g

type family G a where
    G a = ()

main :: IO (G ())
main = return ()
