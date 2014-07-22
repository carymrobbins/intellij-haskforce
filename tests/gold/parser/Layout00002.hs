module Layout00002 where

instance Monad (DocM s) where
        return = retDocM

{-# INLINE thenDocM #-}
{-# INLINE then_DocM #-}

thenDocM :: DocM s a -> (a -> DocM s b) -> DocM s b

