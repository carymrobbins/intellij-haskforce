module Layout00010 where

lexStdToken :: Lex a Token
lexStdToken = do
    case s of
        _               -> 7

      where lexIdents :: Lex a [String]
            lexIdents = do
                exts
