module Data.Conf.Parser (Conf, parseConf) where

import Data.Maybe

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax

type Conf = [(String, String)]

getModule :: ParseResult HsModule -> Maybe HsModule
getModule (ParseOk x) = Just x
getModule _ = Nothing

getDecls :: HsModule -> [HsDecl]
getDecls (HsModule _ _ _ _ ds) = ds

getPair :: HsDecl -> Maybe (String, HsExp)
getPair (HsPatBind _ (HsPVar (HsIdent name)) (HsUnGuardedRhs value) _) = Just (name, value)
getPair _ = Nothing

parseDecls :: String -> [HsDecl]
parseDecls s = maybe [] getDecls $ getModule $ parseModule s

parseConf :: String -> Conf
parseConf s = map (fmap prettyPrint) $ mapMaybe getPair $ parseDecls s
