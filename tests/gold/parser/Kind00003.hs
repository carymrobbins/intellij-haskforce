{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module Kind00003 where


{-
-- FIXME: Waiting on https://github.com/haskell-suite/haskell-src-exts/issues/125
data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: a -> HList t -> HList (a ': t)

-- FIXME: Waiting on https://github.com/haskell-suite/haskell-src-exts/issues/124
data Tuple :: (*,*) -> * where
  Tuple :: a -> b -> Tuple '(a,b)
-}


class C1 (a :: Bool {- 1 -}) where
    c :: proxy a -> Int

class C2 (a :: [ * ])

class C3 (a :: [(Int, Double)])

class C4 (a :: ( * ))

data X (a :: [*])

x1 = undefined :: X '[Int]
x2 = undefined :: X '[Int, Double]

data Y (a :: (*, Bool))

y1 = undefined :: Y '(Double, True)
y2 = undefined :: Y '(Double, 'False {-comment-})
