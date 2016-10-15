module Pragma00005 where

data Test where
  Test :: !Int -> Test

data Test2 where
  Test :: {-# UNPACK #-} !Int -> Test2