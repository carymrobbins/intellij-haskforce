module FunctionUsagesInSingleFile00002 where

-- https://github.com/carymrobbins/intellij-haskforce/issues/89

a :: Integer
a = 1

Just foo = "frog"

b = 2

usage = a + <caret>b
