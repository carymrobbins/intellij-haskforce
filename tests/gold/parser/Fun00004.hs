{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators#-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnboxedTuples #-}
module Fun00004 where

-- | A record with options for explicit passing in rewrite rules.
data FeldOpts = FeldOpts
    { targets    :: [Target]
    }

-- | Default options.
defaultFeldOpts :: FeldOpts
defaultFeldOpts = FeldOpts { targets = [] }

-- | Insert a variable into the environment
localVar :: Typeable b => VarId -> Info b -> Opt a -> Opt a
localVar v info = local $ \env -> env {varEnv = (v, SomeInfo info):varEnv env}

-- | It the expression is a literal, its value is returned, otherwise 'Nothing'
viewLiteral :: forall info dom a. ((Literal :|| Type) :<: dom)
            => ASTF (Decor info (dom :|| Typeable)) a -> Maybe a
viewLiteral (prjF -> Just (C' (Literal a))) = Just a
viewLiteral _ = Nothing

g = (# #)