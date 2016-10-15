module Layout00011 where

compileProg env loc (In (App Ut.Switch _ [tree@(In (App Ut.Condition _ [In (App Ut.Equal _ [_, s]), _, _]))])) = do
    scrutinee <- compileExpr env s
    alts      <- chaseTree env loc s tree
    tellProg [Switch{..}]
