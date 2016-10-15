{-# LANGUAGE  FlexibleContexts #-}
module DungeonMaster.Movement where
import Game.GameState
import Control.Monad.State
import Control.Lens
import Character.Character
import Control.Monad.Trans.Either
import TalismanErrors.TalismanErrors
import Board.Space.Space
import Data.Maybe

movePhase :: (MonadState GameState m, MonadIO m) =>
  ReifiedLens' Players (Maybe Player) -> EitherT TalismanErrors m ()
movePhase playerLens  = do
   currentPlayers <- gets (^. players)
   currentPlayer <- hoistEither $ maybe (Left PlayerNotFound) Right $ currentPlayers ^. runLens playerLens
   let currentPlace = currentPlayer ^.  place
       selectSpaceFunction = currentPlayer ^. ai . selectSpace
   currentDieRolls <- gets ( ^. dieRolls)
   options <- hoistEither $ movementOptions (head currentDieRolls) currentPlace
   board <- gets ( ^. board )
   spaces <- hoistEither $ lookupSpaces options board
   selectedSpace <- liftIO $ selectSpaceFunction spaces
   let selectedSpaceType = selectedSpace ^. spaceType
   newPlayers <- traverseOf (runLens playerLens) (alterMaybePlayer (place .~ selectedSpaceType)) currentPlayers
   modify (\gameState -> gameState
       & players .~ newPlayers
       & dieRolls %~ tail
    )

