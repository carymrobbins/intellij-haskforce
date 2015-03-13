module GoToSymbolFunction_BehaveWhenCaretOutsideAFunction where
<caret>
test :: IO Int
test = do
  seven <- return 7
  return $ seven + 1