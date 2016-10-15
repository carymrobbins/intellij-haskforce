module Layout00001 where

topLexer :: Lex a (Loc Token)
topLexer = do
                 t <- if bol' then lexBOL
                              else lexToken  -- t
                 el <- getSrcLocL
                 return 2
