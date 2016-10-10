{-# LANGUAGE RankNTypes #-}
module Type00003 where

type Run t = forall n b. Monad n => t n b -> n (StT t b)
