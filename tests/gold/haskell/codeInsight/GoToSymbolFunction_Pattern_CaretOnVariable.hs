module GoToSymbolFunction_Pattern_CaretOnVariable where

test :: Int
test = let (seven, eight) = (7,8) in
       s<caret>even + 1