{-# LANGUAGE  TemplateHaskell #-}
module Board.Space.Space where

import Board.Adventure.Adventure
import Board.Follower.Follower
import Board.Object.Object
import Data.Map
import TalismanErrors.TalismanErrors
import Control.Lens
import Data.List as List

class DrawCards a where
  cardsToDraw :: a -> Int

data SpaceType =
  Fields1Space
  | Fields2Space
  | Fields3Space
  | Fields4Space
  | Fields5Space
  | Fields6Space
  | ForestSpace
  | RuinsSpace
  | TavernSpace
  | Plains1Space
  | Plains2Space
  | Plains3Space
  | Plains4Space
  | Woods1Space
  | Woods2Space
  | Woods3Space
  | Hills1Space
  | Hills2Space
  | CitySpace
  | ChapelSpace
  | SentinelSpace
  | GraveyardSpace
  | VillageSpace
  | CragsSpace  deriving (Eq, Ord, Show, Enum)

instance DrawCards SpaceType where
  cardsToDraw Fields1Space = 1
  cardsToDraw Fields2Space = 1
  cardsToDraw Fields3Space  = 1
  cardsToDraw Fields4Space  = 1
  cardsToDraw Fields5Space  = 1
  cardsToDraw Fields6Space  = 1
  cardsToDraw ForestSpace   = 1
  cardsToDraw RuinsSpace    = 1
  cardsToDraw TavernSpace   = 1
  cardsToDraw Plains1Space  = 1
  cardsToDraw Plains2Space  = 1
  cardsToDraw Plains3Space  = 1
  cardsToDraw Plains4Space  = 1
  cardsToDraw Woods1Space   = 1
  cardsToDraw Woods2Space   = 1
  cardsToDraw Woods3Space   = 1
  cardsToDraw Hills1Space   = 1
  cardsToDraw Hills2Space   = 1
  cardsToDraw CitySpace     = 1
  cardsToDraw ChapelSpace   = 1
  cardsToDraw SentinelSpace = 1
  cardsToDraw GraveyardSpace= 1
  cardsToDraw VillageSpace  = 1
  cardsToDraw CragsSpace  = 1

data Space = Space {
  _freeFollowers :: [Follower]
  , _freeObjects :: [Object]
  , _adventures :: [Adventure]
  , _spaceType :: SpaceType
} deriving (Show, Eq)


makeLenses ''Space

instance DrawCards Space where
  cardsToDraw space = cardsToDraw $ space ^. spaceType

type Board = Map SpaceType Space
type BoardLayout = Map SpaceType [SpaceType]

boardLayout :: BoardLayout
boardLayout = fromList [
    (ChapelSpace ,   [Hills1Space, Fields6Space])
  , (Hills1Space ,   [ChapelSpace, SentinelSpace])
  , (SentinelSpace,  [Hills1Space, Woods1Space])
  , (Woods1Space,    [SentinelSpace, GraveyardSpace])
  , (GraveyardSpace, [Woods1Space, Fields1Space])
  , (Fields1Space,   [GraveyardSpace, VillageSpace])
  , (VillageSpace,   [Fields1Space, Fields2Space])
  , (Fields2Space,   [VillageSpace, ForestSpace])
  , (ForestSpace,    [Fields2Space, Plains1Space])
  , (Plains1Space,   [ForestSpace, RuinsSpace])
  , (RuinsSpace,     [Plains1Space, Fields3Space])
  , (Fields3Space,   [RuinsSpace, TavernSpace])
  , (TavernSpace,    [Fields3Space, Plains2Space])
  , (Plains2Space,   [TavernSpace, Woods2Space])
  , (Woods2Space,    [Plains2Space, Plains3Space])
  , (Plains3Space,   [Woods2Space, Hills2Space])
  , (Hills2Space,    [Plains3Space, Fields4Space])
  , (Fields4Space,   [Hills2Space, CitySpace])
  , (CitySpace,      [Fields4Space, Fields5Space])
  , (Fields5Space,   [CitySpace, Woods3Space])
  , (Woods3Space,    [Fields5Space, Plains4Space])
  , (Plains4Space,   [Woods3Space, CragsSpace])
  , (CragsSpace,     [Plains4Space, Fields6Space])
  , (Fields6Space,   [CragsSpace, ChapelSpace])
 ]

{-Initialize previous with current space as it doesn't matter in the beginning anyway, the filtering
should just not happen (in this case it will happen but no space can be it's own neigbour so nothing
will be filtered. It's nasty, it's true. -}
movementOptions :: Int -> SpaceType -> Either TalismanErrors [SpaceType]
movementOptions dieRoll curSpace = movementOptionsEither boardLayout dieRoll curSpace curSpace

movementOptionsEither :: BoardLayout -> Int -> SpaceType -> SpaceType -> Either TalismanErrors [SpaceType]
movementOptionsEither _ 0 previous curSpace = Right [curSpace]
movementOptionsEither layout x previous curSpace = do
    neighbours <- maybe
                     (Left SpaceTypeNotFound)
                     Right $ layout ^. at curSpace
    let neighboursButNoBackTracking = List.filter (/= previous) neighbours
    listOfLists <- traverse (movementOptionsEither layout (x-1) curSpace) neighboursButNoBackTracking
    return . concat $ listOfLists

movementOptionsMaybe :: BoardLayout -> Int -> SpaceType -> SpaceType -> Maybe [SpaceType]
movementOptionsMaybe _ 0 previous curSpace      = Just [curSpace]
movementOptionsMaybe layout x previous curSpace = do
                                    neighbours <- layout ^. at curSpace
                                    let neighboursButNoBackTracking = List.filter (/= previous) neighbours
                                    listOfLists <- traverse (movementOptionsMaybe layout (x-1) curSpace) neighboursButNoBackTracking
                                    return . concat $ listOfLists

createInitialBoard :: Board
createInitialBoard = fromList $ zip fieldsTypeList $
                       fmap createStartingSpace fieldsTypeList
                     where fieldsTypeList = [Fields1Space ..]

createStartingSpace :: SpaceType -> Space
createStartingSpace spaceType = Space {
    _spaceType = spaceType
  , _freeFollowers = []
  , _freeObjects = []
  , _adventures = []
  }

lookupSpaces :: [SpaceType] -> Board -> Either TalismanErrors [Space]
lookupSpaces spaceTypes board = traverse
  (\spaceType -> maybe (Left SpaceTypeNotFound) Right $ board ^. at spaceType)
  spaceTypes

