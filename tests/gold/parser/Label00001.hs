{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
module Label00001 where

import Control.Lens ((^.))
import Data.Generics.Labels ()

data Foo = Foo { bar :: Bar } deriving (Generic)

data Bar = Bar { baz :: Char } deriving (Generic)

example :: Char
example = (Foo (Bar 'a')) ^. #bar . #baz
