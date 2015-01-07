module GoToSymbolFunction_WhereClause where

test :: Int
test = let seven = 7 in
       s<even> + 1
       where eight = 8
             seven = 9

test :: Int
test = s<caret>even + 1
       where seven = 7
             eight = seven + 1
