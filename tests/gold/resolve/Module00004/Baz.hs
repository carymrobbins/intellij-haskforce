module Baz (
    module X
  , module Foo
  ) where

import Hiding as X hiding (foo)
import Qualified as X (foo2)
import Foo
