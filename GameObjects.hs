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
    mainCorridor,
    supplyCabinet,
    universalSpeechTranslator,
    electricBox,
    southCorridorExitDoor,
    engineeringChiefAccessCard,
    woundedEngineeringChief,
    cantineEntranceDoor,
    cantine,
    lockedSafetyBox,
    cyberKey,
    engineRoom,
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
            ("Cantine", cantine),
            ("Engine Room", engineRoom)
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
          securityDoor,
          thickBlanket
        ],
      roomExits = Map.fromList [(East, "Crew Bedroom Vent")]
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
      objectValues = Map.fromList [("pickable", True), ("usable", True)]
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
      roomDescription = "You crawl into a rather spatious crew bedroom vent. There it is! This is where you Desk_Key went! Good thing it didn't fall deeper or you would be stuck in here for ever.",
      roomObjects =
        [ deskKey
        ],
      roomExits = Map.fromList [(West, "Crew Bedroom")]
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
      roomDescription = "You decide it's finally time to leave your quarters. Staying here definetely won't help you find out what's going on. \nYou enter the main corridor, but you can barely see anything. You can see the reason now why the alarms are going off. \nA fire is raging only a few meters in front of you. 'Probably just an overcharged e-box' - your engineer's instinct tells you. However you still can't pass through it. You need to find a way to put it out. \nBehind your back, at the west end, there is a supply cabinet, but the east end of the corridor lays behind the fire. ",
      roomObjects =
        [ supplyCabinet,
          electricBox
        ],
      roomExits = Map.empty
    }

-- Main Corridor Objects
supplyCabinet :: Object
supplyCabinet =
  Object
    { objectName = "Supply_Cabinet",
      objectDescription = "The supply cabinet is wrapped in a chain and locked with a padlock. There is now way there is a key here.",
      objectValues = Map.fromList [("pickable", False), ("openable", False), ("door", False)]
    }

universalSpeechTranslator :: Object
universalSpeechTranslator =
  Object
    { objectName = "Universal_Speech_Translator",
      objectDescription = "A device that translates any language to any other language. Use it on a being to commucate with it.",
      objectValues = Map.fromList [("pickable", True), ("usable", True)]
    }

electricBox :: Object
electricBox =
  Object
    { objectName = "Electric_Box",
      objectDescription = "The electric box is on fire. You need to put it out somehow.",
      objectValues = Map.fromList [("pickable", False), ("usable", False)]
    }

southCorridorExitDoor :: Object
southCorridorExitDoor =
  Object
    { objectName = "South_Corridor_Exit_Door",
      objectDescription = "This exit leads out of the living space to the other sections of the ship. \nIf you want to go through it, you need to unlock it somehow.",
      objectValues = Map.fromList [("pickable", False), ("openable", False), ("door", True)]
    }

engineeringChiefAccessCard :: Object
engineeringChiefAccessCard =
  Object
    { objectName = "Engineering_Chief_Access_Card",
      objectDescription = "The access card of the engineering chief. It can be used to open doors that require an engineering chief access level.",
      objectValues = Map.fromList [("pickable", True), ("usable", True)]
    }

woundedEngineeringChief :: Object
woundedEngineeringChief =
  Object
    { objectName = "Wounded_Engineering_Chief",
      objectDescription = "You: Hey, chief, are you okay? What happened? \nQaux'ods: Glibberflop blorptix zlorgaflar, sploink vroobleshnack glipthor flibberjib!!! \nYou: I can't understand anything. God, if I only knew Luzxorian...",
      objectValues = Map.fromList [("pickable", False), ("usable", False)]
    }

cantineEntranceDoor :: Object
cantineEntranceDoor =
  Object
    { objectName = "Cantine_Entrance_Door",
      objectDescription = "This door leads to the cantine. It is not locked with any lock.",
      objectValues = Map.fromList [("pickable", False), ("openable", True), ("door", True)]
    }

-- Cantine
cantine :: Room
cantine =
  Room
    { roomName = "Cantine",
      roomDescription = "You enter the cantine. You see a Locked_Safety_Box, with wires on the control panel ripped apart. \n",
      roomObjects =
        [ lockedSafetyBox
        ],
      roomExits = Map.empty
    }

-- Cantine Objects
lockedSafetyBox :: Object
lockedSafetyBox =
  Object
    { objectName = "Locked_Safety_Box",
      objectDescription = "The safety box is locked, and the control panel wires are ripped apart \nIt won't open right now, but maybe if you reconnect the wires you could open it \nThere are 3 sockets and 3 cables. You need to connect them in the right order. \n Blue - b \nRed -r \nYellow -y \nYou can connect the wires using connect <cable1> <cable2> <cable3> \n",
      objectValues = Map.fromList [("pickable", False), ("openable", False), ("door", False)]
    }

cyberKey :: Object
cyberKey =
  Object
    { objectName = "Cyber_Key",
      objectDescription = "A key that can open a securely locked door.",
      objectValues = Map.fromList [("pickable", True), ("usable", True)]
    }

-- Engine Room
engineRoom :: Room
engineRoom =
  Room
    { roomName = "Engine Room",
      roomDescription = "You enter the engine room.\n",
      roomObjects =
        [],
      roomExits = Map.fromList [(North, "Main Corridorz")]
    }
