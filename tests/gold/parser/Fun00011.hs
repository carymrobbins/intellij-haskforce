{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DataKinds #-}
module Fun00011 where

data ICon a = a :> b

f :: [] -> ()
f = undefined

g :: a (->) b
g = foo >>= \case
              Just x -> x
              Nothing -> 12

{- FIXME: Add support for MultiWayIf.
h = if | True -> 12
       | False -> 9
-}

i = mdo 7

sort   :: (?cmp :: a -> a -> Bool) => [a] -> [a]
sort    = sortBy ?cmp

clearPage :: ArrPtr 4096 Word8 -> IO ()
clearPage (ArrPtr p) = undefined

instance Has Point "x" Int where from (Point x _) _ = x

y1 = undefined :: Y '()
