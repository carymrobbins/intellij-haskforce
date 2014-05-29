module Fun00008 where

f x = let z = if x then 3 else 8
      in 19

main2 = do
            h <- ask
            let m = f h
            13
            return m
