module DerivingStrategies00001 where

data Foo = Foo
  deriving stock (Bar)

data Foo = Foo
  deriving anyclass (Bar)

data Foo = Foo
  deriving stock (Bar)
  deriving anyclass (Bar)


newtype Foo = Foo Int
  deriving stock (Bar)

newtype Foo = Foo Int
  deriving newtype (Bar)

newtype Foo = Foo Int
  deriving anyclass (Bar)

newtype Foo = Foo Int
  deriving stock (Bar)
  deriving newtype (Bar)
  deriving anyclass (Bar)
