{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TransformListComp #-}

module Fun00012 where

data instance A = M { unA :: T -> A }

data instance B where
   K :: D -> B

f = [ (x, y) | x <- xs,
               y <- ys,
               then sortWith ]

g = [ (x, y) | x <- xs,
               y <- ys,
               then sortWith by (x + y) ]

h = [ (x, y) | x <- xs,
               y <- ys,
               then group using permutations ]

i = [ (x, y) | x <- xs,
               y <- ys,
               then group by (x + y) using groupWith ]

{-# RULES "f" f True = False #-}