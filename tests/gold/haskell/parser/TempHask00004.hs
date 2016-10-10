{-# LANGUAGE QuasiQuotes #-}
module TempHask00004 where

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]
