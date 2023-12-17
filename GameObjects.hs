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
    workshop,
    alien_mass,
    engineering_chief_office_door,
    hand_saw,
    electrical_tools,
    wooden_table_leg,
    workshop_window,
    small_fire,
    escape_pods,
    table,
    broken_console,
    closed_computer,
    metal_statue,
    escape_pod_launch_console,
    makeshift_torch,
    opened_computer,
    code_1867,
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
import qualified Data.Map.Strict as Map
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
            ("workshop", workshop),
            ("engineering_chief_office", engineering_chief_office)
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


workshop :: Room
workshop =
    Room
    { roomName = "workshop",
      roomDescription = "The workshop is where most engineering on the station happens. \n\
      \A door North leads to the engineering chiefs office, an opening South to the escape pods",
      roomObjects =
        [alien_mass, engineering_chief_office_door, toolbox, workshop_window, small_fire, table],
      --roomExits = Map.fromList [(West, engine_room)]
      roomExits = Map.empty
    }



alien_mass :: Object
alien_mass = Object "alien_mass" "A strange black mass near the *workshop_window* blocks the path south. It pulsates slightly, as if breathing.\n\
  \Underneath it you see one of your collegues being slowly absorbed by what you assume to be some kind of alien intruder. \n\
    \A familiar smell of fuel fumes seems to be eminating from the creature.\n\
    \It migth be flammable" (Map.fromList [("pickable", False)])

engineering_chief_office_door :: Object
engineering_chief_office_door = 
  Object "engineering_chief_office_door" 
  "The door to the chief's office, usually locked by an access card"
  (Map.fromList [("pickable", False), ("openable", False), ("door", True)])


toolbox :: Object
toolbox = 
  Object "toolbox" "Standard-issue toolbox. It's unlocked" (Map.fromList [("pickable", False), ("openable", True), ("door", False)])

hand_saw :: Object
hand_saw = 
  Object "hand_saw" "An old hand saw" (Map.fromList [("pickable", True), ("usable", True)])

electrical_tools :: Object
electrical_tools =
  Object "electrical_tools" "Various tools for electrical work such as wire cutters, soldering iron etc."
  (Map.fromList [("pickable", True), ("usable", True)])



wooden_table_leg :: Object
wooden_table_leg = 
  Object "wooden_table_leg" "Table leg, wooden" (Map.fromList [("pickable", True), ("usable", True)])
  


 -- TODO: HANDLE destroying
workshop_window :: Object
workshop_window = 
  Object "workshop_window" "You look at the window and into space. You see pieces of debris coming from the ship as well as some strange black round objects you can't identify\n\
   \Can be broken with enough force. Last time this happened 2 workers got sucked out into space."
   (Map.fromList [("pickable", False)])

small_fire :: Object
small_fire = 
  Object "small_fire" "A small electical fire seems to have broken out in the corner of the room"
  (Map.fromList [("pickable", False)])


table :: Object
table = 
  Object "table" "An old wooden table. One of its legs seems to be barely holding on. \n\
  \You might be able to detach it if you had the proper tool."
  (Map.fromList [("pickable", False)])


engineering_chief_office :: Room
engineering_chief_office =
  Room {
    roomName = "engineering_chief_office",
    roomDescription = "The office is in heavy dissaray. A closed_computer sits on the desk. Next to one of the bookshelves lays a broken glass table. Something heavy must've fallen on it from one of the shelves.", 
    roomObjects = [closed_computer, metal_statue],
    roomExits = Map.fromList [(South, "workshop")]
  }

engineering_chief_access_card :: Object
engineering_chief_access_card = Object "engineering_chief_access_card" "a card" (Map.fromList [("pickable", True), ("usable", True)])

closed_computer :: Object
closed_computer = Object "closed_computer" "A closed computer" (Map.fromList [("pickable", False), ("openable", True), ("door", False)])

opened_computer :: Object
opened_computer = Object "opened_computer" "You open the closed_computer sitting on the desk. You find an open email titled ESCAPE POD CODE UPDATE: \n\
\ Hi, Qaux'ods, please remember about the annual escape pod tests. We've changed all the codes to *1867* for this week to make the process easier. \n\
\ Please have the report done by next week. Cheers." (Map.fromList [("pickable", False)])


metal_statue :: Object  
metal_statue = Object "metal_statue" "A heavy metal statue seems to have fallen down from one of the shelves and broken through a glass table. \n\
\It's just small enough for you to pick up and seems to be some kind of award given to the engineering chief." (Map.fromList [("pickable", True), ("usable", True)])

escape_pods :: Room  
escape_pods =
  Room {
    roomName = "escape_pods",
    roomDescription = "This room is designed to hold the emergency evacuation modules for the engineering staff.\n\
    \All of them have either already been deployed, or are now covered in an alien, dark grey substance similar to the one that blocked the entrance to this room.\n\
     \All except for one. You have to move fast. The pods must first be lowered using the console.\n\
      \Then, once inside one of the pods, access to launch has to be granted by entering a code known to the managers of a given branch of the station.",
    roomObjects = [broken_console],
    roomExits = Map.fromList [(North, "workshop")]
  }

broken_console :: Object  
broken_console =
  Object "broken_console" "A console used for lowering the escape pods, broken. Looks like it short-circuted. \n\
  \You spot some black matter between the wires. This must be what caused the break.\n\
   \Needs specialised tools to be fixed."
  (Map.fromList [("pickable", False)])


escape_pod_launch_console :: Object
escape_pod_launch_console = 
  Object "escape_pod_launch_console" "Inside the pod is a big screen with a prompt that reads: \n\
  \PLEASE ENTER LAUNCH AUTHORISATION CODE TO INITIATE LAUNCH SEQUENCE"
  (Map.fromList [("pickable", False)])


makeshift_torch :: Object
makeshift_torch = 
  Object "makeshift_torch" "A torch fashioned from a wooden table leg." (Map.fromList [("pickable", True), ("usable", True)])

code_1867 :: Object
code_1867 =
  Object "code_1867" "A code to a numerical lock" (Map.fromList [("pickable", True), ("usable", True)])
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
