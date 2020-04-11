#!/usr/bin/env stack
{- stack
  --install-ghc runghc
  --resolver lts-14.12
  --package aeson
  --package bytestring
  --package hashable
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
import Data.Aeson (FromJSON(parseJSON), FromJSONKey, (.:))
import Data.Aeson.Types (FromJSONKey)
import Data.Coerce (Coercible, coerce)
import Data.Either (partitionEithers)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.String.Interpolate as Interpolate
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import qualified System.IO as IO

main :: IO ()
main = do
  parserYaml <- getParserYaml
  schema :: Schema <- Yaml.decodeFileThrow parserYaml
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

getParserYaml :: IO FilePath
getParserYaml = getArgs >>= \case
  [parserYaml] -> pure parserYaml
  args -> do
    IO.hPutStrLn IO.stderr ("Invalid arguments: " <> show args)
    exitFailure

newtype Schema = Schema (HashMap ElementName ElementDef)
  deriving stock (Show, Eq)
  deriving newtype (FromJSON)

newtype ElementName = ElementName Text
  deriving stock (Show, Eq)
  deriving newtype (FromJSONKey, Hashable)

newtype ElementDef = ElementDef [ElementAttr]
  deriving stock (Show, Eq)

instance FromJSON ElementDef where
  parseJSON =
    Aeson.withObject "ElementDef" $ fmap ElementDef . traverse go . HashMap.toList
    where
    go (k, v) = do
      when (Text.null k) $ fail "Invalid empty key found"
      if Char.isUpper (Text.head k) then
        ElementSubtype . SubtypeDef (ElementName k) <$> parseJSON v
      else
        ElementAttrMethod (MethodName k) <$> parseJSON v

newtype MethodName = MethodName Text
  deriving stock (Show, Eq)
  deriving newtype (FromJSONKey, Hashable)

data ElementAttr =
    ElementAttrMethod MethodName MethodDef
  | ElementSubtype SubtypeDef
  deriving stock (Show, Eq)

data SubtypeDef = SubtypeDef ElementName (HashMap MethodName MethodDef)
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

#{Text.intercalate "\n" $ render p <$> HashMap.toList m}
}
|]

instance Render Psi Schema where
  render p (Schema m) = [Interpolate.i|
object HaskellParser2020Psi {
  trait HElement extends PsiElement

  type HTokenElement[A] = PsiElement
#{Text.intercalate "\n\n" $ render p <$> HashMap.toList m}
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

#{Text.intercalate "\n\n" $ render p <$> HashMap.toList m}
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
#{Text.intercalate "\n" $ map renderCases (HashMap.toList m)}
    }
  }
}
|]
    where
    renderCases (name, ElementDef attrs) =
      Text.intercalate "\n" $
        if null subtypeNames then
          [renderCase (text name)]
        else
          map renderCase subtypeNames
      where
      subtypeNames = flip mapMaybe attrs $ \case
        ElementSubtype (SubtypeDef name _) -> Just (text name)
        _ -> Nothing

    renderCase name = stripNewlines [Interpolate.i|
      case Elements.#{toUpperSnake name} => new PsiImpl.#{name}Impl(node)
|]

instance Render Elements (ElementName, ElementDef) where
  render p (topName, ElementDef attrs) =
    -- We don't render the 'top' type if we have subtypes since the 'top' type
    -- in those cases is abstract.
    if (Text.null renderSubs) then renderTop else renderSubs
    where
    topUpperSnakeName = toUpperSnake topName

    renderTop = renderObj topUpperSnakeName

    renderObj n = "\n  object " <> n <> " extends HElementType(\"" <> n <> "\")"

    renderSubs = foldMap renderSub subtypes

    renderSub (SubtypeDef name _) = renderObj (toUpperSnake name)

    (_, subtypes) = flip partitionWith attrs $ \case
      ElementAttrMethod name def -> Left (name, def)
      ElementSubtype def -> Right def

instance Render Psi (ElementName, ElementDef) where
  render p (topName, ElementDef attrs) =
    renderTopTrait <> renderSubTraits
    where
    renderTopTrait = [Interpolate.i|
  trait #{text topName} extends HElement {
#{topBody}
  }
|]

    topBody = foldMap (render p) topMethods

    (topMethods, subtypes) = flip partitionWith attrs $ \case
      ElementAttrMethod name def -> Left (name, def)
      ElementSubtype def -> Right def

    renderSubTraits =
      foldMap (render p . (topName,)) subtypes

instance Render PsiImpl (ElementName, ElementDef) where
  render p (topName, ElementDef attrs) =
    -- We don't render the 'top' type if we have subtypes since the 'top' type
    -- in those cases is abstract.
    if Text.null renderSubClasses then renderTopClass else renderSubClasses
    where
    renderTopClass = [Interpolate.i|
  class #{text topName}Impl(node: ASTNode) extends HElementImpl(node) with Psi.#{text topName} {
#{foldMap (render p) topMethods}
  }
|]

    (topMethods, subtypes) = flip partitionWith attrs $ \case
      ElementAttrMethod name def -> Left (name, def)
      ElementSubtype def -> Right def

    renderSubClasses =
      Text.intercalate "\n\n" $ fmap (render p) subtypes

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

instance Render Psi (ElementName, SubtypeDef) where
  render p (supertypeName, SubtypeDef name methods) = [Interpolate.i|
  trait #{text name} extends #{text supertypeName} {
#{foldMap (render p) (HashMap.toList methods)}
  }
|]

instance Render PsiImpl SubtypeDef where
  render p (SubtypeDef name methods) = [Interpolate.i|
  class #{text name}Impl(node: ASTNode) extends HElementImpl(node) with Psi.#{text name} {
#{foldMap (render p) (HashMap.toList methods)}
  }
|]

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
