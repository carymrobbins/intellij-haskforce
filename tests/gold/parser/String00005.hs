module String00005 where

instance (Show e) => Show (UntypedFeldF e) where
   show (Variable v)                = show v
   show (Lambda v e)                = "(\\" ++ show v ++ " -> " ++ show e ++ ")"
