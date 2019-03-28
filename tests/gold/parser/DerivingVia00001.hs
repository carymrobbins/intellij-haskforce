module DerivingVia00001 where

newtype Foo = Foo Bar
  deriving stock (Generic, Eq, Show)
  deriving anyclass (Spam)
  deriving (ToJSON, FromJSON) via Bar

deriving via Eggs instance ToJSON Baz
