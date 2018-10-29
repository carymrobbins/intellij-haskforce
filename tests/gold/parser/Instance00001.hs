module Instance00001 where

instance {-# OVERLAPPABLE #-} Foo a => Bar a where
  foo = bar
