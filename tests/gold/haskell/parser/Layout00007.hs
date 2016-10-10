module Layout00007 where

lexStdToken = do
    return s
      where lexIdents = do
                case s of
                 _ | XmlSyntax `elem` exts -> do
                        return $ ident : idents
                 _ | MagicHash `elem` exts -> do
                        return "#"
