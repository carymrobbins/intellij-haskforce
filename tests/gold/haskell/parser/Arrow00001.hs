{-# LANGUAGE Arrows #-}
module Arrow00001 where

e1 = proc x -> do
        rec let e = e + 1
        y <- f -< x+1
        g -<< 2*y
        let z = x+y
        t <- h >>- x*z
        returnA -< t+z

expr' = proc x -> do
                returnA -< x
        <+> do
                symbol Plus -< ()
                y <- term >- ()
                expr' -< x + y
        <+> do
                symbol Minus -< ()
                y <- term -< ()
                expr' -< x - y
