module Fun00006 where

rev :: [a] -> [a]
rev xs = go [] xs
    where go acc []     = acc
          go acc (x:xs) = go (x:acc) xs
