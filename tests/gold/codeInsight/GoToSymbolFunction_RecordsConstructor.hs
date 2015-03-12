module GoToSymbolFunction_RecordsConstructor where

data Pool a = Pool (Maybe a)

getResource :: Pool a -> Maybe a
getResource (P<caret>ool maybeA) = maybeA