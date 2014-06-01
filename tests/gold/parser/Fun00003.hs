{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}

module Fun00003 where

instance Show (Info a)
  where
    show i@(Info {}) = show (infoType i) ++ szStr ++ srcStr
      where
        szStr = case show (infoSize i) of
          "AnySize" -> ""
          str       -> " | " ++ str

        srcStr = case infoSource i of
          ""  -> ""
          src -> " | " ++ src

data Set (cxt :: * -> (* -> *)) a = Set [a]

g2 :: (forall a. Eq a => [a] -> a -> Bool) -> Int -> Int
g2 f x | Just m <- f undefined
       , True
       = x

f = \(x::Int, y::a) -> x
g (x::a) = x
h ((x,y) :: (Int,Bool)) = (y,x)

fac (n+1) = (n+1) * fac n

foc (-1) = 3

lazyFunc, strictFunc :: () -> ()

lazyFunc ~() = ()

strictFunc !v = ()

firstZero (head -> 0) = True