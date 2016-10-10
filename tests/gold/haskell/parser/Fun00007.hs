{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
module Fun00007 where

-- | Cast a 'Size' to a 'RangeSet'
sizeToRange :: forall a . Type a => Size a -> RangeSet a
sizeToRange sz = case typeRep :: TypeRep a of
    IntType _ _ -> RangeSet sz
    _           -> Universal

deriving instance Typeable (,,,,,,,,,,,,,,)
