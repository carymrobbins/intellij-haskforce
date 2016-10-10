module Layout00015 where

decodeType :: String -> [Type]
decodeType s = goL s []
  where
    goL [] acc = reverse acc
    goL s acc = goL rest' (out:acc)
       where (out, rest) = go s
             rest' = case rest of
                      '_':t -> t
                      _     -> rest

    go (stripPrefix "void"     -> Just t) = (VoidType, t)
    go (stripPrefix "bool"     -> Just t) = (MachineVector 1 BoolType, t)
