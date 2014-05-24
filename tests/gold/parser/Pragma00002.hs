module Pragma00002 where

data Vector a = Pull DIM1  {-# UNPACK #-} !a

