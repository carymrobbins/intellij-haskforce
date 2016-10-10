{-# LANGUAGE MagicHash #-}
module MagicHash00001 where

-- Ripped from Data.List, verify I# constructor is valid.

findIndices      :: (a -> Bool) -> [a] -> [Int]
#ifdef USE_REPORT_PRELUDE
findIndices p xs = [ i | (x,i) <- zip xs [0..], p x]
#else
-- Efficient definition
findIndices p ls = loop 0# ls
                 where
                   loop _ [] = []
                   loop n (x:xs) | p x       = I# n : loop (n +# 1#) xs
                                 | otherwise = loop (n +# 1#) xs
#endif  /* USE_REPORT_PRELUDE */

