module TempHask00001 where

rewriteType :: Type -> Q Type
rewriteType t = applyTF ''Internal t >>= expandTF
