-- NOTE: This file is auto-generated.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP                #-}
-- | Module to be shared between server and client.
--
-- This module must be valid for both GHC and Fay.
module Language.Fay.Yesod where

import           Prelude
#ifdef FAY
import           FFI
#else
import           Fay.FFI
#endif
import           Data.Data

#ifdef FAY

data Text = Text
    deriving (Show, Read, Eq, Typeable, Data)

fromString :: String -> Text
fromString = ffi "%1"

toString :: Text -> String
toString = ffi "%1"

#else

import qualified Data.Text as T

type Text = T.Text

fromString :: String -> Text
fromString = T.pack

toString :: Text -> String
toString = T.unpack

#endif
