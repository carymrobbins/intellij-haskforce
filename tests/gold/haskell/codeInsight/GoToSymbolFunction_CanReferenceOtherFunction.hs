module GoToSymbolFunction_CanReferenceOtherFunction where

test2 :: Int -> Int
test2 x = x

test :: IO Int
test = do
  seven <- return $ t<caret>est2 7
  return $ seven + 1