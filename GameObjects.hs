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


workshop :: Room
workshop =
    Room
    { roomName = "workshop",
      roomDescription = "The workshop is where most engineering on the station happens.",
      roomObjects =
        [alien_mass],
      roomExits = Map.fromList [(South, initialRoom)]
    }


alien_mass :: Object
alien_mass = Object "alien_mass" "A strange black mass near the *workshop_window* blocks the path south. It pulsates slightly, as if breathing.\n
  Underneath it you see one of your collegues being slowly absorbed by what you assume to be some kind of alien intruder. \n
    A familiar smell of fuel fumes seems to be eminating from the creature.\n
    It migth be flammable" (Map.fromList [("pickable", False)])







