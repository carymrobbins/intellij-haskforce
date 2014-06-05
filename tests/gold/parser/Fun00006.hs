module Fun00006 where

import Prelude hiding ((++), (.))

rev :: [a] -> [a]
rev xs = go [] xs
    where go acc []     = acc
          go acc (x:xs) = go (x:acc) xs

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x:(xs ++ ys)

(+++) = (++)

(f . g) x = f (g x)

infixr 5 ++
infix 7 :+
infixl 9 +++
infix 3 .