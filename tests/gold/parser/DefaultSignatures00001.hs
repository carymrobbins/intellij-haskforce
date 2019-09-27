module DefaultSignatures00001 where

class Foo a where
  foo :: a -> String
  default foo :: (Show a) => a -> String
  foo = show
