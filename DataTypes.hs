module DataTypes where

import Data.List
import Data.Map.Strict qualified as Map

-- Define a data type to represent directions
data Direction = North | South | West | East
  deriving (Show, Eq, Ord)

data Object = Object
  { objectName :: String,
    objectDescription :: String,
    objectValues :: Map.Map String Bool
  }
  deriving (Show)

-- Define a data type to represent rooms
data Room = Room
  { roomName :: String,
    roomDescription :: String,
    roomObjects :: [Object],
    roomExits :: Map.Map Direction String
  }
  deriving (Show)

-- Define a data type to represent the game state
data GameState = GameState
  { currentRoom :: Room,
    inventory :: [Object],
    allRooms :: Map.Map String Room
  }
  deriving (Show)
