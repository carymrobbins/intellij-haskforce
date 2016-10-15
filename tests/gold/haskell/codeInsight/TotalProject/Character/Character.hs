{-# LANGUAGE TemplateHaskell #-}
module Character.Character where

import Board.Object.Object
import Board.Follower.Follower
import Board.Adventure.Adventure
import Control.Lens
import Board.Space.Space

data Alignment = Good | Neutral | Evil deriving (Eq, Ord, Show)

data CharacterType = Wizard | Thief | OgreChieftain deriving (Eq, Ord, Show)

data Character = Character {
  _strength :: Int
  , _craft :: Int
  , _fate :: Int
  , _gold :: Int
  , _life :: Int
  , _objects :: [Object]
  , _followers :: [Follower]
  , _alignment :: Alignment
  , _strengthTrophies :: [Int]
  , _craftTrophies :: [Int]
  , _characterType :: CharacterType
} deriving (Show, Eq)

makeLenses ''Character

sillyAI :: AI
sillyAI = AI {
  _selectCharacter = return . Just .  head
  , _selectSpace = return . head
  , _selectAttackType = return $ Lens strength
}

{- Maybe place has to move to Character??-}
data Player = Player {
  _character :: Character
  , _place :: SpaceType
  , _ai :: AI
}

instance Show Player where
  show player = show (_character player) ++ " " ++ show (_place player)

instance Eq Player where
   (==) player1 player2 = _character player1 == _character player2
                           && _place player1 == _place player2

data AI = AI {
  _selectCharacter :: [Player] -> IO (Maybe Player)
  , _selectSpace :: [Space] -> IO Space
  , _selectAttackType :: IO (ReifiedLens' Character Int)

}

makeLenses ''Player

makeLenses ''AI


wizard :: AI -> Player
wizard ai = Player {
  _character = Character {
    _strength=2,
    _craft=5,
    _fate=3,
    _gold=1,
    _life=4,
    _objects=[],
    _followers=[],
    _alignment=Evil,
    _strengthTrophies = [],
    _craftTrophies = [],
    _characterType = Wizard
    }
  , _place = GraveyardSpace
  , _ai = ai
}

ogreChieftain::AI -> Player
ogreChieftain ai = Player {
  _character = Character {
    _strength=5,
    _craft=2,
    _fate=1,
    _gold=1,
    _life=6,
    _objects=[],
    _followers=[],
    _alignment=Neutral,
    _strengthTrophies = [],
    _craftTrophies = [],
    _characterType = OgreChieftain
    }
  , _place=CragsSpace
  , _ai = ai
}

thief :: AI -> Player
thief ai = Player {
  _character = Character {
    _strength=3,
    _craft=3,
    _fate=2,
    _gold=1,
    _life=4,
    _objects=[],
    _followers=[],
    _alignment=Neutral,
    _strengthTrophies = [],
    _craftTrophies = [],
    _characterType = Thief
    }
  , _place= CitySpace
  , _ai = ai
}

class DetermineAttackType a where
  determineAttackType :: a -> IO (ReifiedLens' Character Int)

instance DetermineAttackType Player where
  determineAttackType p =
    case p ^. character . characterType of
      Wizard -> p ^. ai . selectAttackType
      _ -> return $ Lens strength