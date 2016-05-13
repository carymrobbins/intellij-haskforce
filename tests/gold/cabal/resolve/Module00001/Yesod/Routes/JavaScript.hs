{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Yesod.Routes.<resolved>JavaScript where

import Control.Monad.Reader (ask)
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Text (Text, pack, intercalate, toLower)
import Yesod.Core.Content
import Yesod.Core.Types
import Yesod.Routes.TH.Types

class JSRoutable a where
    jsRoutes :: a -> Text

getJSRoutesR :: JSRoutable a => HandlerT a IO TypedContent
getJSRoutesR = fmap (TypedContent "text/javascript" . toContent . jsRoutes) ask

jsRoutesBuilder :: [ResourceTree String] -> b -> Text
jsRoutesBuilder = const . buildJSRoutes

buildJSRoutes :: [ResourceTree String] -> Text
buildJSRoutes resourcesApp = sep <> var <> sep <> intercalate sep resources <> sep
  where
    prefix = "jsRoutes"
    resources = map (buildResource prefix) resourcesApp
    var = "var " <> prefix <> "={}"
    sep = ";"

buildResource :: Text -> ResourceTree String -> Text
buildResource prefix (ResourceLeaf (Resource{resourceDispatch=Methods{..}, ..})) =
    prefix <> "." <> pack resourceName <> "={" <> resource <> "}"
  where
    sep = ","
    resource = intercalate sep (map (buildMethod resourcePieces) methodsMethods)

-- TODO: Handle subsites and parents, possibly with Yesod.Routes.TH.Types.flatten?
buildResource _ (ResourceLeaf (Resource{resourceDispatch=Subsite{..}, ..})) = "/* Subsite not supported */"
buildResource _ ResourceParent{} = "/* ResourceParent not supported */"

buildMethod :: [Piece String] -> String -> Text
buildMethod pieces methodName = method <> ":function(){return {method:\"" <> method <> "\",url:\"" <> buildUrl pieces <> "\"};}"
  where
    method = toLower . pack $ methodName

buildUrl :: [Piece String] -> Text
buildUrl = snd . foldl' action (0 :: Int, "/")
  where
    sep = "/"
    action (counter, t) piece = case piece of
        Static s -> (counter, t <> pack s <> sep)
        Dynamic _ -> (counter + 1, t <> "\"+arguments[" <> (pack . show) counter <> "]+\"" <> sep)
