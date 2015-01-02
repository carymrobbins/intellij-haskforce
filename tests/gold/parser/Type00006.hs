module Type00006 where

newtype MaybeK a = MaybeK { runMaybeK :: forall r. (a -> Maybe r) -> Maybe r }
