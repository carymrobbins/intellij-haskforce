{-# LANGUAGE MagicHash, FlexibleContexts, MultiParamTypeClasses, TypeFamilies, DataKinds #-}
module Eta00001 where

data {-# CLASS "java.util.Collection" #-} Collection a = Collection (Object# (Collection a))
data {-# CLASS "java.util.ArrayList" #-} ArrayList a = ArrayList (Object# (ArrayList a))
data {-# CLASS "java.util.List" #-} List a = List (Object# (List a))
data {-# CLASS "java.lang.Integer" #-} JInteger = JInteger (Object# JInteger)
data {-# CLASS "java.io.PrintStream" #-} PrintStream = PrintStream (Object# PrintStream)

instance Class (ArrayList a) where
  obj = ArrayList
  unobj (ArrayList o) = o

instance Class (List a) where
  obj = List
  unobj (List o) = o

instance Class (Collection a) where
  obj = Collection
  unobj (Collection o) = o

instance Class JInteger where
  obj = JInteger
  unobj (JInteger o) = o

instance Class PrintStream where
  obj = PrintStream
  unobj (PrintStream o) = o

type instance Inherits (ArrayList a) = '[List a]
type instance Inherits (List a) = '[Collection a]
type instance Inherits JInteger = '[Object]

foreign import java unsafe "@new" newInteger :: Int -> JInteger
foreign import java unsafe "intValue" intValue :: JInteger -> Int
foreign import java unsafe "@new" newArrayList :: IO (ArrayList a)
foreign import java unsafe "add" add :: (Extends a Object, Extends b (Collection a)) => a -> Java b Bool
foreign import java unsafe "get" get :: (Extends a Object, Extends b (List a)) => Int -> Java b a
foreign import java unsafe "@static @field java.lang.System.out" stdout' :: PrintStream
foreign import java unsafe "println" println :: Extends a Object => PrintStream -> a -> IO ()