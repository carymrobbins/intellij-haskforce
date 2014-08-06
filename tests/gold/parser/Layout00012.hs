module Layout00012 where

type BoundedInt a = (BoundedSuper a, BoundedSuper (UnsignedRep a))

-- | Super class to 'BoundedInt'
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
class    (Ord a, Num a, Bounded a, Integral a, FiniteBits a) => BoundedSuper a
#endif
