module Board.Adventure.Adventure where

data Adversary = Monster Int | Spirit Int deriving (Show, Eq)

data Adventure = Combat Adversary deriving (Show, Eq)

