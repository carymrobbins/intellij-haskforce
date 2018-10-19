{-# LANGUAGE LambdaCase #-}
module LambdaCase00001 where

unJust = \case
  Just x -> x
  Nothing -> error "so unjust!"

unJust' = \ case
  Just x -> x
  Nothing -> error "so unjust!"

unJust'' = map $ \ case
  Just x -> x
  Nothing -> error "so unjust!"
