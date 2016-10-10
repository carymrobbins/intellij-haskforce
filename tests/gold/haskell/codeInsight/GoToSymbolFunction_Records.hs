module GoToSymbolFunction_Records where

data Pool a = Pool (Maybe a)

getResource :: Pool a -> Maybe a
getResource (Pool maybeA) = m<caret>aybeA