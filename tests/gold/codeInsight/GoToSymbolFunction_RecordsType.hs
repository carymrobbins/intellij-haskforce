module GoToSymbolFunction_RecordsType where

data Pool a = Pool (Maybe a)

getResource :: P<caret>ool a -> Maybe a
getResource (Pool maybeA) = maybeA