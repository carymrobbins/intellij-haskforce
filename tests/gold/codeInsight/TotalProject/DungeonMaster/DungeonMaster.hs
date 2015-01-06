{-# LANGUAGE  FlexibleContexts #-}
module DungeonMaster.DungeonMaster where

import Control.Monad.State
import Control.Monad.Trans.Either
import Character.Character
import Board.Space.Space
import Control.Lens
import qualified Data.List as List
import Data.Map
import Game.GameState
import Board.Adventure.Adventure
import Data.Foldable
import TalismanErrors.TalismanErrors
import DungeonMaster.Movement
import DungeonMaster.Fight

nextRoll:: (MonadState [DieRoll] m , Monad m) => m DieRoll
nextRoll = do
    rolls <- get
    let nextOne = head rolls
    put $ tail rolls
    return nextOne




determineNumberOfCardsToDraw :: (MonadState GameState m, MonadIO m) =>
  ReifiedLens' Players (Maybe Player) -> EitherT TalismanErrors m Int
determineNumberOfCardsToDraw playerLens = do
  players <- gets (^. players)
  currentPlayer <- maybe (left PlayerNotFound) right $ players ^. runLens playerLens
  let currentSpaceLens  = currentPlayer ^. place
  board <- gets (^.  board)
  currentSpace <- hoistEither $ maybe (Left SpaceTypeNotFound) Right $  board ^. at currentSpaceLens
  let maxCards = cardsToDraw currentSpace
  let currentCards =  length $ currentSpace ^. adventures
  case maxCards `compare` currentCards of
    GT -> return $ maxCards - currentCards
    _ -> return 0

alterMaybeSpace :: (MonadState GameState m) => (Space -> Space) ->
  Maybe Space -> EitherT TalismanErrors m (Maybe Space)
alterMaybeSpace alterSpace maybeSpace =
  hoistEither $ maybe (Left SpaceTypeNotFound) (Right .Just . alterSpace) maybeSpace

addAdventures :: (MonadState GameState m) => ReifiedLens' Board (Maybe Space)
  -> [Adventure] -> EitherT TalismanErrors m ()
addAdventures  spaceLens newAdventures = do
  currentBoard <- gets ( ^. board)
  newBoard <-  traverseOf (runLens spaceLens)
    (alterMaybeSpace (adventures %~ (newAdventures ++))) currentBoard
  modify (\gameState -> board .~ newBoard $ gameState )


drawCards :: (MonadState GameState m, MonadIO m) => ReifiedLens' Players (Maybe Player) -> EitherT TalismanErrors m ()
drawCards playerLens = do
  numberOfCardsToDraw <- determineNumberOfCardsToDraw  playerLens
  currentPlayers <- gets (^. players)
  currentAdventureStack <- gets ( ^. adventureStack)
  let (drawnCards, rest) = splitAt numberOfCardsToDraw currentAdventureStack
  modify (adventureStack .~ rest)
  currentPlayer <- maybe (left PlayerNotFound) right $ currentPlayers ^. runLens playerLens
  addAdventures (Lens $ at $  currentPlayer ^. place ) drawnCards

playRound :: (MonadState GameState m, MonadIO m) => ReifiedLens' Players (Maybe Player)
 -> EitherT TalismanErrors m ()
playRound playerLens = do
  movePhase playerLens
  players <- gets (^. players)
  player <- maybe (left PlayerNotFound) right $ players ^.  runLens playerLens
  otherPlayersOnSameSpace <- getOtherPlayersInSamePosition playerLens
  fightPhase player otherPlayersOnSameSpace
  drawCards playerLens


test :: Int
test = let a = 7 in
       a + 3

test2 :: IO Int
test2 = do
  a <- return 7
  let b = a + 3
  return b