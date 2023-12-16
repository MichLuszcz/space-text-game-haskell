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
        [alien_mass, engineering_chief_office_door, toolbox, workshop_window, small_fire, table],
      roomExits = Map.fromList [(South, initialRoom)]
    }



-- TODO: add destroy handling
alien_mass :: Object
alien_mass = Object "alien_mass" "A strange black mass near the *workshop_window* blocks the path south. It pulsates slightly, as if breathing.\n
  Underneath it you see one of your collegues being slowly absorbed by what you assume to be some kind of alien intruder. \n
    A familiar smell of fuel fumes seems to be eminating from the creature.\n
    It migth be flammable" (Map.fromList [("pickable", False)])

--TODO: add opening with engineering_chief_access_card
engineering_chief_office_door :: Object
engineering_chief_office_door = 
  Object "engineering_chief_office_door" 
  "The door to the chief's office, usually locked by an access card"
  (Map.fromList [("pickable", False), ("openable", False)])


-- TODO: Add open handling for toolbox
toolbox :: Object
toolbox = 
  Object "toolbox" "Standard-issue toolbox. It's unlocked" (Map.fromList [("pickable", False), ("openable", True)])

 -- TODO: HANDLE destroying
workshop_window :: Object
workshop_window = 
  Object "workshop_window" "You look at the window and into space. You see pieces of debris coming from the ship as well as some strange black round objects you can't identify\n
   Can be broken with enough force. Last time this happened 2 workers got sucked out into space."
   (Map.fromList [("pickable", False)])

-- TODO handle lighting on fire
small_fire :: Object
small_fire = 
  Object "small_fire" "A small electical fire seems to have broken out in the corner of the room"
  (Map.fromList [("pickable", False)])


-- TODO handle breaking table
table :: Object
table = 
  Object "table" "An old wooden table. One of its legs seems to be barely holding on. \n
  You might be able to detach it if you had the proper tool."
  (Map.fromList [("pickable", False)])

