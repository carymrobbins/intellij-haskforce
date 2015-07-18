module Baz (
    module Foo.Foo
  , abc
  ) where

import Foo.Foo

abc = "xyz"
