{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MagicHash #-}
module Fun00009 where

instance Sized a => Sized (Digit a) where
       {-# SPECIALIZE instance Sized (Digit (Elem a)) #-}
       {-# SPECIALIZE instance Sized (Digit (Node a)) #-}
       size xs = foldl (\ i x -> i + size x) 0 xs

f x = case x of
       Just x | null x -> 9.3
       Nothing -> 7.1

data (a1 :< a2)  = Foo


f1 = let x :: Float#
         x = 10.2#
         y :: Double#
         y = 10.7##
         z :: Int#
         z = 1#
         a :: Word#
         a = 9##
         b :: Char#
         b = 'B'#
         c :: String#
         c = "Hello, world!"#
     in 19