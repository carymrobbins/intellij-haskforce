module Pragma00001 where

type Vector a = Pull DIM1 a
{-# DEPRECATED Vector "Use Pull instead" #-}
