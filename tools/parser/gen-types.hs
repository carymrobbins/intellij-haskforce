#!/usr/bin/env stack
{- stack
  --install-ghc runghc
  --resolver lts-14.12
  --package aeson
  --package bytestring
  --package hashable
  --package process
  --package string-interpolate
  --package text
  --package unordered-containers
  --package yaml
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (when)
import Data.Aeson (FromJSON(parseJSON), (.:), FromJSONKey)
import Data.Aeson.Types (FromJSONKey)
import Data.Coerce (Coercible, coerce)
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust, mapMaybe)
import Data.Ord (comparing)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitFailure, exitSuccess)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.String.Interpolate as Interpolate
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import qualified System.IO as IO
import qualified System.IO.Unsafe
import qualified System.Process as Proc

main :: IO ()
main = do
  schema <- readSchema =<< getParserYaml
  Text.putStrLn $ stripNewlinesStart [Interpolate.i|
////////////////////////////////////////////
// THIS IS A GENERATED FILE; DO NOT EDIT! //
////////////////////////////////////////////

package com.haskforce.haskell.lang.parser.gen

import java.util

import com.haskforce.haskell.lang.parser.gen.{HaskellParser2020Elements => Elements}
import com.haskforce.haskell.lang.parser.gen.{HaskellParser2020Psi => Psi}
import com.haskforce.haskell.lang.parser.gen.{HaskellParser2020PsiImpl => PsiImpl}
import com.haskforce.haskell.lang.parser.{HaskellTokenTypes2020 => T}
import com.haskforce.HaskellLanguage
import com.haskforce.psi.HaskellTokenType
import com.intellij.extapi.psi.ASTWrapperPsiElement
import com.intellij.lang.ASTNode
import com.intellij.psi.PsiElement
import com.intellij.psi.tree.IElementType
import com.intellij.psi.util.PsiTreeUtil

import scala.reflect.ClassTag

#{render (Proxy :: Proxy Elements) schema}

#{render (Proxy :: Proxy Psi) schema}

#{render (Proxy :: Proxy PsiImpl) schema}

#{render (Proxy :: Proxy Factory) schema}
|]

-- | Gets the path to the parser yaml file as a command line argument.
getParserYaml :: IO FilePath
getParserYaml = getArgs >>= \case
  [parserYaml] -> pure parserYaml
  args -> do
    IO.hPutStrLn IO.stderr ("Invalid arguments: " <> show args)
    exitFailure

-- | Used to set the 'globalKeyOrder'. Reads the yaml file to determine the order of
-- keys in the file so we can preserve them.
getKeyOrder :: FilePath -> IO (HashMap Text Int)
getKeyOrder parserYaml = do
  runBash "grep -io '^ *[a-z][a-z]*' \"$1\" | tr -d ' '" [parserYaml] <&> \case
    (ExitSuccess, out, _err) ->
      HashMap.fromList $ zip (map Text.pack $ lines out) [0..]

    (ExitFailure n, out, err) ->
      error $ "getKeyOrder failed with code " <> show n
        <> "; stdout: <> " <> show out
        <> "; stderr: <> " <> show err

  where
  runBash expr args = Proc.readProcessWithExitCode "bash" ("-c" : expr : "bash" : args) ""

-- | Used in 'FromJSON' instances so we can preserve the order of keys as they
-- exist in the yaml file. See 'globalKeyOrder' and 'getKeyOrder'.
keySortOn :: (a -> Text) -> [a] -> [a]
keySortOn f = List.sortBy $ comparing $ (globalKeyOrder HashMap.!) . f

{-# NOINLINE globalKeyOrder #-}
globalKeyOrder :: HashMap Text Int
globalKeyOrder = fromJust $ System.IO.Unsafe.unsafePerformIO $ readIORef globalKeyOrderRef

{-# NOINLINE globalKeyOrderRef #-}
globalKeyOrderRef :: IORef (Maybe (HashMap Text Int))
globalKeyOrderRef = System.IO.Unsafe.unsafePerformIO $ newIORef Nothing

initGlobalKeyOrder :: HashMap Text Int -> IO ()
initGlobalKeyOrder = writeIORef globalKeyOrderRef . Just

readSchema :: FilePath -> IO Schema
readSchema parserYaml = do
  initGlobalKeyOrder =<< getKeyOrder parserYaml
  Yaml.decodeFileThrow parserYaml

newtype Schema = Schema [(ElementName, ElementDef)]
  deriving stock (Show, Eq)

instance FromJSON Schema where
  parseJSON v = go <$> parseJSON v
    where
    go :: HashMap ElementName ElementDef -> Schema
    go = Schema . keySortOn (text . fst) . HashMap.toList

newtype ElementName = ElementName Text
  deriving stock (Show, Eq)
  deriving newtype (FromJSONKey, Hashable)

newtype ElementDef = ElementDef [ElementAttr]
  deriving stock (Show, Eq)

collectMethods :: ElementDef -> [(MethodName, MethodDef)]
collectMethods (ElementDef attrs) = attrs >>= \case
  ElementAttrMethod name def -> [(name, def)]
  _ -> []

collectSubtypes :: ElementDef -> [(ElementName, ElementDef)]
collectSubtypes (ElementDef attrs) = attrs >>= \case
  ElementSubtype name def -> [(name, def)]
  _ -> []

instance FromJSON ElementDef where
  parseJSON =
    Aeson.withObject "ElementDef" $ \o -> do
      attrs :: [ElementAttr] <- traverse go $ HashMap.toList o
      pure $ ElementDef $ keySortOn sortKey attrs
    where
    go :: (Text, Aeson.Value) -> Aeson.Parser ElementAttr
    go (k, v) = do
      when (Text.null k) $ fail "Invalid empty key found"
      if Char.isUpper (Text.head k) then do
        ElementDef attrs <- parseJSON v
        pure
          $ ElementSubtype (ElementName k)
          $ ElementDef
          $ keySortOn sortKey attrs
      else do
        ElementAttrMethod (MethodName k) <$> parseJSON v

    sortKey = \case
      ElementAttrMethod (MethodName k) _ -> k
      ElementSubtype (ElementName k) _ -> k

newtype MethodName = MethodName Text
  deriving stock (Show, Eq)
  deriving newtype (FromJSONKey, Hashable)

data ElementAttr =
    ElementAttrMethod MethodName MethodDef
  | ElementSubtype ElementName ElementDef
  deriving stock (Show, Eq)

newtype TypeExpr = TypeExpr Text
  deriving stock (Show, Eq)
  deriving newtype (FromJSON)

newtype Impl = Impl Text
  deriving stock (Show, Eq)
  deriving newtype (FromJSON)

data MethodDef = MethodDef
  { final :: Bool
  , typ :: TypeExpr
  , impl :: Maybe Impl
  } deriving stock (Show, Eq)

instance FromJSON MethodDef where
  parseJSON = \case
    Aeson.String s ->
      pure MethodDef
        { final = False
        , typ = TypeExpr s
        , impl = Nothing
        }

    v ->
      flip (Aeson.withObject "MethodDef") v $ \o -> do
        final <- o .: "final"
        typ   <- o .: "type"
        impl  <- o .: "impl"
        pure MethodDef { final, typ, impl }

data RenderType = Elements | Psi | PsiImpl | Factory

class Render (t :: RenderType) a where
  render :: Proxy t -> a -> Text

instance Render Elements Schema where
  render p (Schema m) = [Interpolate.i|
object HaskellParser2020Elements {

  sealed abstract class HElementType(name: String) extends IElementType(name, HaskellLanguage.INSTANCE)

#{Text.intercalate "\n" $ map (render p) m}
}
|]

instance Render Psi Schema where
  render p (Schema m) = [Interpolate.i|
object HaskellParser2020Psi {
  trait HElement extends PsiElement

  type HTokenElement[A] = PsiElement
#{Text.intercalate "\n\n" $ render p <$> m}
}
|]

instance Render PsiImpl Schema where
  render p (Schema m) = [Interpolate.i|
object HaskellParser2020PsiImpl {

  import Psi._

  abstract class HElementImpl(node: ASTNode) extends ASTWrapperPsiElement(node) with Psi.HElement {
    override def toString: String = node.getElementType.toString

    protected def one[A <: Psi.HElement](implicit ct: ClassTag[A]): A = {
      notNullChild(PsiTreeUtil.getChildOfType[A](this, cls[A]))
    }

    protected def option[A <: Psi.HElement](implicit ct: ClassTag[A]): Option[A] = {
      Option(PsiTreeUtil.getChildOfType[A](this, cls[A]))
    }

    protected def list[A <: Psi.HElement](implicit ct: ClassTag[A]): util.List[A] = {
      PsiTreeUtil.getChildrenOfTypeAsList[A](this, cls[A])
    }

    protected def oneTok(t: HaskellTokenType): PsiElement = {
      notNullChild(findChildByType[PsiElement](t))
    }

    //noinspection SameParameterValue
    protected def optionTok(t: HaskellTokenType): Option[PsiElement] = {
      Option(findChildByType[PsiElement](t))
    }
  }

  private def cls[A](implicit ct: ClassTag[A]): Class[A] = {
    ct.runtimeClass.asInstanceOf[Class[A]]
  }

#{Text.intercalate "\n\n" $ render p <$> m}
}
|]

instance Render Factory Schema where
  render p (Schema m) = [Interpolate.i|
object HaskellParser2020Factory {

  def createElement(node: ASTNode): PsiElement = {
    node.getElementType match {
      case t: Elements.HElementType => createHElement(node, t)
      case t => throw new AssertionError(s\"Unexpected element type: $t\")
    }
  }

  private def createHElement(node: ASTNode, t: Elements.HElementType): Psi.HElement = {
    t match {
#{Text.intercalate "\n" $ map renderCases m}
    }
  }
}
|]
    where
    renderCases (name, def) =
      Text.intercalate "\n"
        $ map renderCase
        $ text name `ifEmpty` subtypeNames def

    subtypeNames (ElementDef attrs) = attrs >>= \case
      ElementSubtype name def -> text name : subtypeNames def
      _ -> []

    renderCase name = stripNewlines [Interpolate.i|
      case Elements.#{toUpperSnake name} => new PsiImpl.#{name}Impl(node)
|]

instance Render Elements (ElementName, ElementDef) where
  render p (topName, ElementDef topAttrs) =
    renderLeaves
    where
    -- We don't render the 'top' type if we have subtypes since the 'top' type
    -- in those cases is abstract.
    renderLeaves =
      foldMap renderName $
        topName `ifEmpty` leafNames topAttrs

    topUpperSnakeName = toUpperSnake topName

    renderTop = renderObj topUpperSnakeName

    renderObj n = "\n  object " <> n <> " extends HElementType(\"" <> n <> "\")"

    renderName name = renderObj (toUpperSnake name)

    leafNames attrs = attrs >>= \case
      ElementAttrMethod _ _ -> []
      ElementSubtype name (ElementDef attrs') ->
        name `ifEmpty` leafNames attrs'

instance Render Psi (ElementName, ElementDef) where
  render p (topName, topDef) =
    renderTopTrait <> renderSubTraits
    where
    topBody = foldMap (render p) $ collectMethods topDef

    renderSubTraits =
      foldMap (render p . (`Extends` topName)) $ collectSubtypes topDef

    renderTopTrait = [Interpolate.i|
  trait #{text topName} extends HElement {
#{topBody}
  }
|]

data Extends = (ElementName, ElementDef) `Extends` ElementName

instance Render Psi Extends where
  render p ((name, def) `Extends` superName) = [Interpolate.i|
  trait #{text name} extends #{text superName} {
#{foldMap (render p) $ collectMethods def}
  }
|]

instance Render PsiImpl (ElementName, ElementDef) where
  render p (topName, topDef) =
    -- We don't render the 'top' type if we have subtypes since the 'top' type
    -- in those cases is abstract.
    if Text.null renderSubClasses then renderTopClass else renderSubClasses
    where
    renderSubClasses =
      Text.intercalate "\n\n" $ map (render p) $ collectSubtypes topDef

    renderTopClass = [Interpolate.i|
  class #{text topName}Impl(node: ASTNode) extends HElementImpl(node) with Psi.#{text topName} {
#{foldMap (render p) $ collectMethods topDef}
  }
|]

instance Render Psi (MethodName, MethodDef) where
  render p (name, MethodDef { final, typ, impl }) =
    "\n    " <> go
    where
    defOnly = "def " <> text name <> ": " <> text typ
    go =
      if final then
        case impl of
          Nothing -> error $ "In " <> show name <> ", 'final: true' requires impl"
          Just x -> "final " <> defOnly <> " = " <> text x
      else
        defOnly

instance Render PsiImpl (MethodName, MethodDef) where
  render p (name, methodDef@MethodDef { final, typ }) =
    if final then "" else go
    where
    go = "\n    override def " <> text name <> ": " <> text typ <> " = " <> body

    typText = text typ :: Text

    typePrefixIs = flip Text.isPrefixOf typText

    body =
      if typePrefixIs "Option[HTokenElement[" then
        "optionTok(" <> extractTok "Option[HTokenElement[" ".type]]" <> ")"
      else if typePrefixIs "HTokenElement[" then
        "oneTok(" <> extractTok "HTokenElement[" ".type]" <> ")"
      else if typePrefixIs "Option[" then
        "option"
      else if typePrefixIs "util.List[" then
        "list"
      else if "[" `Text.isInfixOf` typText then
        error $ "Unsupported type detected for " <> show name <> ": " <> show methodDef
      else
        "one"

    extractTok p s =
      maybe onFailure id $ Text.stripSuffix s =<< Text.stripPrefix p typText
      where
      onFailure =
        error $
          "Unexpected tok type: " <> show typ
            <> "; expected prefix=" <> show p
            <> "; expected suffix=" <> show s

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith f xs = partitionEithers $ map f xs

-- Converts "FooBar" to "FOO_BAR"
toUpperSnake :: (Coercible a Text) => a -> Text
toUpperSnake t = Text.pack $ viaString $ Text.unpack $ coerce t
  where
  viaString s = dropWhile (== '_') $ do
    c <- s
    if Char.isUpper c then ['_', Char.toUpper c] else [Char.toUpper c]

text :: (Coercible a Text) => a -> Text
text = coerce

stripNewlinesStart :: Text -> Text
stripNewlinesStart = Text.dropWhile (== '\n')

stripNewlines :: Text -> Text
stripNewlines = Text.dropWhile (== '\n') . Text.dropWhileEnd (== '\n')

ifEmpty :: (Foldable f, Applicative f) => a -> f a -> f a
ifEmpty a fa = if null fa then pure a else fa
