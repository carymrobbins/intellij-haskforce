module String00005 where

instance (Show e, Show f) => Show (UntypedFeldF e f) where
   show (Variable v t)              = 'm':(show v `append` show t)
   show (Lambda v e)                = "(\\" ++ show v ++ " -> " ++ show e ++ ")"

default (Integer, Double)