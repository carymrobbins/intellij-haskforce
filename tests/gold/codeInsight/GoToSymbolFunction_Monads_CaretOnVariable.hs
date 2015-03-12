module GoToSymbolFunction_Monads_CaretOnVariable where

test :: IO Int
test = do
  seven <- return 7
  return $ s<caret>even + 1