module ForAll00001 where

data BlockedFetch r = forall a. BlockedFetch (r a) (ResultVar a)