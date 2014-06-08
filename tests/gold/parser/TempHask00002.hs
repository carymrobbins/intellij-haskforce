{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module TempHask00002 where

[undefined|
something here "hello world"
|]

[| 7 |]

[d| 7 |]

[t| Int |]

[p| 7 |]

$(deriveStuff 'f)
m = $x

''T

f n = \ [haskell|y|] -> y+n
