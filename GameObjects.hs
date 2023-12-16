module GameObjects (initialState, nextRoom, initialRoom) where

import Data.List
import Data.Map.Strict qualified as Map
import DataTypes

-- Example game initialization
initialRoom :: Room
initialRoom =
  Room
    { roomName = "Dark Room",
      roomDescription = "You find yourself in a dark room.",
      roomObjects =
        [ Object "key" "A shiny golden key." (Map.fromList [("pickable", True), ("usable", True)]),
          Object "table" "A wooden table." (Map.fromList [("pickable", False)]),
          Object
            "North-Door"
            "It's a door leading out of the room on the north side. It's closed right now, but you can open it."
            (Map.fromList [("pickable", False), ("openable", False)])
        ],
      roomExits = Map.empty
    }

nextRoom :: Room
nextRoom =
  Room
    { roomName = "Next Room",
      roomDescription = "A mysterious room.",
      roomObjects =
        [Object "book" "An old dusty tome." (Map.fromList [("pickable", True)])],
      roomExits = Map.fromList [(South, initialRoom)]
    }

initialState :: GameState
initialState =
  GameState
    { currentRoom = initialRoom,
      inventory = [],
      allRooms = Map.fromList [("Dark Room", initialRoom), ("Next Room", nextRoom)]
    }
