module Layout00018 where

getDB :: [String]
getDB = return dirs
  where
    local   = do
      maybe (return []) f (maybeHasLibs flatten)
{-# NOINLINE getDB #-}

f :: String
f = "Hello"
