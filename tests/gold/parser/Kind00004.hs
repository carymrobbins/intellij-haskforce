{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Kind00004 where

data T :: (k -> *) -> k -> * where
  MkT :: m a -> T Maybe (m a) -> T m a

data T2 :: (k -> (* -> *)) -> k -> * where
  MkT2 :: m a -> T Maybe (m a) -> T m a

data T3 :: (k -> !) -> k -> * where
  MkT3 :: m a -> T Maybe (m a) -> T m a

data T4 :: (k -> (!, *)) -> k -> * where
  MkT4 :: m a -> T Maybe (m a) -> T m a

data T5 :: (k -> (!, *)) -> k -> * where
  MkT5 :: m a -> T Maybe (m a) -> T m a

data T6 :: (k -> [!, !,!]) -> k -> * where
  MkT6 :: m a -> T Maybe (m a) -> T m a

data T7 :: (k -> [!, !,!]) k -> * where
  MkT7 :: m a -> T Maybe (m a) -> T m a
