module GameObjects
  ( initialState,
    crewBedroomVent,
    accessCard,
    thickBlanket,
    crewBedroom,
    securityDoor,
    desk,
    bed,
    locker,
    deskKey,
    hammer,
  )
where

import Data.List
import Data.Map.Strict qualified as Map
import DataTypes

-- Initial GameState
initialState :: GameState
initialState =
  GameState
    { currentRoom = crewBedroom,
      inventory = [],
      allRooms =
        Map.fromList
          [ ("Crew Bedroom", crewBedroom),
            ("Crew Bedroom Vent", crewBedroomVent),
            ("Main Corridor", mainCorridor)
          ]
    }

-- Crew Bedroom
crewBedroom :: Room
crewBedroom =
  Room
    { roomName = "Crew Bedroom",
      roomDescription = "You are in the crew bedroom, there is a security door on the south, and a vent entrance on the east.",
      roomObjects =
        [ desk,
          bed,
          locker,
          securityDoor
        ],
      roomExits = Map.fromList [(West, "Crew Bedroom Vent")]
    }

-- Crew Bedroom Objects
desk :: Object
desk =
  Object
    { objectName = "Desk",
      objectDescription = "The desk is locked. There should be a key on it, but maybe it flew somewhere during the crash.",
      objectValues = Map.fromList [("pickable", False), ("openable", False), ("door", False)]
    }

accessCard :: Object
accessCard =
  Object
    { objectName = "Crew_Access_Card",
      objectDescription = "Crew Access Card used for opening doors that require a certain access level.",
      objectValues = Map.fromList [("pickable", True), ("usable", True)]
    }

bed :: Object
bed =
  Object
    { objectName = "Bed",
      objectDescription = "There is nothing special on this bed, aside from a thick_blanket. I wonder if it could be useful later.",
      objectValues = Map.fromList [("pickable", False), ("openable", False), ("door", False)]
    }

thickBlanket :: Object
thickBlanket =
  Object
    { objectName = "Thick_Blanket",
      objectDescription = "A thick wooly blanket.",
      objectValues = Map.fromList [("pickable", True), ("openable", False), ("door", False)]
    }

locker :: Object
locker =
  Object
    { objectName = "Locker",
      objectDescription = "The locker is closed, but there is no lock on it.",
      objectValues = Map.fromList [("pickable", False), ("openable", True), ("door", False)]
    }

hammer :: Object
hammer =
  Object
    { objectName = "Hammer",
      objectDescription = "A hammer. It looks like it could be used to break things.",
      objectValues = Map.fromList [("pickable", True), ("usable", True)]
    }

securityDoor :: Object
securityDoor =
  Object
    { objectName = "Security_Door",
      objectDescription = "The door leading out of your room to the main corridor remains locked. You need your own *crew_access_card* to unlock it.",
      objectValues = Map.fromList [("pickable", False), ("openable", False), ("door", True)]
    }

-- Crew Bedroom Vent
crewBedroomVent :: Room
crewBedroomVent =
  Room
    { roomName = "Crew Bedroom Vent",
      roomDescription = "You crawl into a rather spatious crew bedroom vent. There it is! This is where you Desk_key went! Good thing it didn't fall deeper or you would be stuck in here for ever.",
      roomObjects =
        [ deskKey
        ],
      roomExits = Map.fromList [(East, "Crew Bedroom")]
    }

-- Crew Bedroom Vent Objects
deskKey :: Object
deskKey =
  Object
    { objectName = "Desk_Key",
      objectDescription = "A key to the desk in the crew bedroom.",
      objectValues = Map.fromList [("pickable", True), ("usable", True)]
    }

-- Main Corridor
mainCorridor :: Room
mainCorridor =
  Room
    { roomName = "Main Corridor",
      roomDescription = "You are in the main corridor. There is a security door on the north, and a door to the south.",
      roomObjects =
        [],
      roomExits = Map.empty
    }
