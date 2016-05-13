module Data.Conf (Conf, readConf, getConf, parseConf) where

import Control.Monad
import Text.Read
import Data.Conf.Parser

getConf :: Read a => String -> Conf -> Maybe a
getConf key conf = lookup key conf >>= readMaybe

readConf :: String -> IO [(String, String)]
readConf = liftM parseConf . readFile
