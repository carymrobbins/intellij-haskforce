module FunctionUsagesInSingleFile00001 where

boring :: ()
boring = const () (funny 18)

funny :: Integer -> Boolean
funny<caret> _ = True
