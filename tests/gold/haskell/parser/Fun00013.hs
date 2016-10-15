module Fun00013 where

data Manifest sh a = Syntax a => Manifest (Data [Internal a]) (Data [Length])
