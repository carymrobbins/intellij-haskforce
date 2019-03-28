module Gold.Parser.Layout00026 where

import Data.Text (Text)
import GHC.Generics
import Foo.Types (Id)
import Bar.Types (Person)
import qualified Data.Aeson as Aeson

data Elem = Elem
  { name :: Text
  , person :: (Id Person)
  } deriving stock (Show, Generic)
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

type Foo f = Baz (Spam f)
type Bar f = Baz (Eggs f)

data Baz v = Baz
  { value :: v
  , children :: [Baz v]
  } deriving stock (Show, Generic)
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data Spam f
  = Spam1 (f Person)
  | Spam2 (f Char)
  deriving stock (Generic)

deriving instance
  ( Show (f Person)
  , Show (f Char)
  ) => Show (Spam f)

aesonOptionsSpam :: Aeson.Options
aesonOptionsSpam = Aeson.defaultOptions
  { Aeson.constructorTagModifier = drop $ length @[] "Spam" }
