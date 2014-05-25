module Fun00005 where

prjF :: Project (sub :|| Type) sup => sup sig -> Maybe ((sub :|| Type) sig)
prjF = prj

class
    ( AlphaEq dom dom (dom :|| Typeable) [(VarId, VarId)]
    , AlphaEq dom dom (Decor Info (dom :|| Typeable)) [(VarId, VarId)]
    , EvalBind dom
    , (Literal :|| Type) :<: dom
    , Typed dom
    , Render dom -- For debug
    , Constrained dom
    , Optimize dom dom
    ) =>
      OptimizeSuper dom

optimizeM :: (OptimizeSuper dom)
          => FeldOpts -> ASTF (dom :|| Typeable) a -> Opt (ASTF (Decor Info (dom :|| Typeable)) a)
optimizeM opts a
    | Dict <- exprDict a
    = constFold <$> matchTrans (\(C' x) -> optimizeFeat opts x) a
