module Record00001 where

data MyRecord = MyRecord { foo :: String, bar :: String }

setFoo :: MyRecord -> String -> MyRecord
setFoo x s = x { foo = s }

setFooAndBar x s1 s2 = (setFoo x s) { bar = s2 }
