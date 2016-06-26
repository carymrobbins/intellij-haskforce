module Operator00002 ((Prelude.:->)) where

import qualified Lib

f x = case x of
  y :-> _ -> y

f x = case x of
  y Lib.:-> _ -> y
