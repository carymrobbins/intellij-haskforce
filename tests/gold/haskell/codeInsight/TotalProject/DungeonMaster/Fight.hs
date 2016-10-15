{-# LANGUAGE FlexibleContexts #-}

module DungeonMaster.Fight where

import Game.GameState
import Control.Lens
import Character.Character
import Board.Adventure.Adventure
import Control.Monad.State
import Control.Monad.Trans.Either
import TalismanErrors.TalismanErrors

determineAttackValue :: (MonadState GameState m) => ReifiedLens' Players (Maybe Player) ->
 ReifiedLens' Character Int -> EitherT TalismanErrors m Int
determineAttackValue playerLens attackType = do
  currentDieRolls <- gets (^. dieRolls)
  currentPlayers <- gets (^. players)
  modify ( & dieRolls %~ tail )
  currentPlayer <- maybe (left PlayerNotFound) right $ currentPlayers ^. runLens playerLens
  let currentPlayerStrength =  currentPlayer ^. character . runLens attackType
  return $ currentPlayerStrength + head currentDieRolls

loseFight :: (MonadState GameState m) => ReifiedLens' Players (Maybe Player) -> EitherT TalismanErrors m ()
loseFight playerLens = do
  currentPlayers <- gets ( ^. players)
  currentPlayer <- maybe (left PlayerNotFound) right $ currentPlayers ^. runLens playerLens
  let charType = currentPlayer ^. character . characterType
  newPlayers <- traverseOf (at charType)
    (alterMaybePlayer (character . life %~ (1 `subtract`))) currentPlayers
  modify (\gameState -> gameState & players .~ newPlayers )



fightPlayer :: (MonadIO m, MonadState GameState m) =>  ReifiedLens' Players (Maybe Player) ->
  ReifiedLens' Players (Maybe Player) -> EitherT TalismanErrors m ()
fightPlayer attackerLens defenderLens = do
  player <- findPlayer attackerLens
  attackType <- liftIO $ determineAttackType player
  attackerAttackStrength <-  determineAttackValue attackerLens attackType
  defenderAttackStrength <-  determineAttackValue defenderLens attackType
  case compare attackerAttackStrength defenderAttackStrength of
    EQ -> return ()
    LT -> loseFight attackerLens
    GT -> loseFight defenderLens

fightAdversary :: (MonadIO m, MonadState GameState m) =>  ReifiedLens' Players (Maybe Player)
  -> Adversary -> EitherT TalismanErrors m ()
fightAdversary playerLens adversary = do
  let (fightType, adversaryBaseAttackValue) = case adversary of
                                            Monster x -> (Lens strength, x)
                                            Spirit x -> (Lens craft, x)
  playerAttackValue <- determineAttackValue playerLens fightType
  currentDieRolls <- gets (^. dieRolls)
  let adversaryAttackValue = adversaryBaseAttackValue + head currentDieRolls
  case compare playerAttackValue adversaryAttackValue of
    EQ -> return ()
    LT -> loseFight playerLens
    GT -> claimTrophy playerLens adversary

claimTrophy :: (MonadState GameState m) =>  ReifiedLens' Players (Maybe Player) -> Adversary
  -> EitherT TalismanErrors m ()
claimTrophy playerLens adversary = do
  currentPlayer <-findPlayer playerLens
  let (trophyLens, value) = case adversary of
                              Monster x -> (strengthTrophies, x)
                              Spirit x -> (craftTrophies, x)
  let updatedPlayer = (character . trophyLens) %~ (value :) $ currentPlayer
  u<caret>pdatePlayer updatedPlayer

test :: (MonadState GameState m) =>  Player -> EitherT TalismanErrors m ()
test = updatePlayer


fightPhase :: (MonadIO m, MonadState GameState m) =>
  Player -> [Player] -> EitherT TalismanErrors m ()
fightPhase player others = do
  maybeFight <- liftIO $ (player ^. ai . selectCharacter) others
--   maybe do this with a traverse in the future, but okay for now
  maybe (return ()) undefined maybeFight
