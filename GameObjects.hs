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
            ("Main Corridor", mainCorridor),
            ("escape_pods", escape_pods),
            ("workshop", workshop)
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


workshop :: Room
workshop =
    Room
    { roomName = "workshop",
      roomDescription = "The workshop is where most engineering on the station happens.",
      roomObjects =
        [alien_mass, engineering_chief_office_door, toolbox, workshop_window, small_fire, table],
      --roomExits = Map.fromList [(West, engine_room)]
      roomExits = Map.empty
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


toolbox :: Object
toolbox = 
  Object "toolbox" "Standard-issue toolbox. It's unlocked" (Map.fromList [("pickable", False), ("openable", True)])

hand_saw :: Object
hand_saw = 
  Object "hand_saw" "An old hand saw" (Map.fromList [("pickable", True), ("usable", True))

electrical_tools :: Object
electrical_tools =
  Object "electrical_tools" "Various tools for electrical work such as wire cutters, soldering iron etc."
  (Map.fromList [("pickable", True), ("usable", True)])



wooden_table_leg :: Object
wooden_table_leg = 
  Object "wooden_table_leg" "Table leg, wooden" (Map.fromList [("pickable", True), ("usable", True))
  


 -- TODO: HANDLE destroying
workshop_window :: Object
workshop_window = 
  Object "workshop_window" "You look at the window and into space. You see pieces of debris coming from the ship as well as some strange black round objects you can't identify\n
   Can be broken with enough force. Last time this happened 2 workers got sucked out into space."
   (Map.fromList [("pickable", False)])

small_fire :: Object
small_fire = 
  Object "small_fire" "A small electical fire seems to have broken out in the corner of the room"
  (Map.fromList [("pickable", False)])


table :: Object
table = 
  Object "table" "An old wooden table. One of its legs seems to be barely holding on. \n
  You might be able to detach it if you had the proper tool."
  (Map.fromList [("pickable", False)])


escape_pods :: Room  
escape_pods =
  Room {
    roomName = "escape_pods",
    roomDescription = "This room is designed to hold the emergency evacuation modules for the engineering staff.\n
    All of them have either already been deployed, or are now covered in an alien, dark grey substance similar to the one that blocked the entrance to this room.\n
     All except for one. You have to move fast. The pods must first be lowered using the console.\n
      Then, once inside one of the pods, access to launch has to be granted by entering a code known to the managers of a given branch of the station.",
    roomObjects = [broken_console],
    roomExits = Map.fromList [(North, "workshop")]
  }

broken_console :: Object  
broken_console =
  Object "broken_console" "A console used for lowering the escape pods, broken. Looks like it short-circuted. \n
  You spot some black matter between the wires. This must be what caused the break.\n
   Needs specialised tools to be fixed."
  (Map.fromList [("pickable", False)])
