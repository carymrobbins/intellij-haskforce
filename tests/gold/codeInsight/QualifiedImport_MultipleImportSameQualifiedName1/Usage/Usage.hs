module Usage.Usage where

import qualified Definition.Definition as Definition
import qualified Other.Definition as Definition


test :: Int
test = D<caret>efinition.seven + 1


testBis :: Int
testBis = Definition.eight + 8

