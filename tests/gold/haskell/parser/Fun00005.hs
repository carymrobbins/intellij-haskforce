{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
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

optimizeN :: (OptimizeSuper dom, t1 ~ t2)
          => FeldOpts -> ASTF (dom :|| Typeable) a -> Opt (ASTF (Decor Info (dom :|| Typeable)) a)

optimizeO :: ()
          => FeldOpts -> ASTF (dom :|| Typeable) a -> Opt (ASTF (Decor Info (dom :|| Typeable)) a)

instance  Bounded a  => ([a] :|| b) where

class GMapKey k where
  data GMap k :: * -> *

instance (GMapKey a, GMapKey b) => GMapKey (Either a b) where
    data GMap (Either a b) v = GMapEither (GMap a v) (GMap b v)

instance Eq e => Collects [e] where
  type Elem [e]   = e
  empty           = []

