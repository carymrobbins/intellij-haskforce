{-# LANGUAGE GADTs #-}
module Fun00001 where

data Maybe a = Nothing | Just a

type Perhaps a = Maybe a