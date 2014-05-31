{-# LANGUAGE TupleSections #-}
module Fun00008 where

f x = let z = if x then (3^) else (^8)
      in -1

main2 = do
            h <- ask
            let m = f h
            13
            (,,3)
            return m

g x = xs ++ ys ++ zs ++ zs'
  where xs = [1..]
        ys = [1..x]
        zs = [1, x ..]
        zs' = [1, x ..(30*x)]

h xs = [y | Just y <- xs] ++ [(y, z) | Just y <- xs | z <- xs ]
