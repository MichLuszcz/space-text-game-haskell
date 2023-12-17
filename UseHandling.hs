module UseHandling where

import Control.Monad.RWS.Lazy (MonadState (put))
import Data.List
import qualified Data.Map.Strict as Map
import DataTypes
import Distribution.Compat.Lens (use)
import GameObjects
import ObjectManagment
import Text.XHtml (target)

-- Function to handle the use command like openDoor from DoorHandling.hs
useItem :: String -> String -> GameState -> (Maybe GameState, Maybe String)
useItem itemName targetName gameState =
  if itemName == "9911" && targetName == "Locked_Crate"
    then openCrate gameState
    else case findObject itemName (inventory gameState) of
      Just obj -> case Map.lookup "usable" (objectValues obj) of
        Just True ->
          -- Make a case of from the itemName and targetName, and the cases should call separate functions like keyUnlockNorthDoor
          case (itemName, targetName) of
            ("hand_saw", "table") -> sawOffTableLeg gameState
            ("Desk_Key", "Desk") -> keyUnlock itemName targetName gameState
            ("Crew_Access_Card", "Security_Door") -> keyUnlock itemName targetName gameState
            ("wooden_table_leg", "small_fire") -> set_leg_on_fire gameState
            ("makeshift_torch", "alien_mass") -> set_alien_on_fire gameState
            ("engineering_chief_access_card", "engineering_chief_office_door") -> keyUnlock itemName targetName gameState
            ("electrical_tools", "broken_console") -> fixConsole gameState
            ("code_1867", "escape_pod_launch_console") -> finishGame gameState
            ("metal_statue", "workshop_window") -> suckAlienMass gameState
            ("Hammer", "Supply_Cabinet") -> hammerCabinet gameState
            ("Thick_Blanket", "Electric_Box") -> putOutFire gameState
            ("Universal_Speech_Translator", "Wounded_Engineering_Chief") -> translateChief gameState
            ("Cyber_Key", "South_Corridor_Exit_Door") -> keyUnlock itemName targetName gameState
            ("Cyber_Key", "Control_Panel") -> lowerBridge gameState
            ("UV_Flashlight", "Locked_Crate") -> flashOnCrate gameState
            ("Ladder", "Bridge_Gap") -> useLadder gameState
            _ -> (Nothing, Just ("You can't figure out how to use " ++ itemName ++ " on " ++ targetName ++ ".\n"))
        Just False -> (Nothing, Just "This item cannot be used.")
        _ -> (Nothing, Just "This item cannot be used.")
      Nothing -> (Nothing, Just "Item not found")

-- Create a keyUnlock function that takes a doorName and a GameState sets the value of "openable" to True for the doorName
keyUnlock :: String -> String -> GameState -> (Maybe GameState, Maybe String)
keyUnlock keyName doorName gameState =
  let updatedRoom = (currentRoom gameState) {roomObjects = map (\obj -> if objectName obj == doorName then obj {objectValues = Map.insert "openable" True (objectValues obj)} else obj) (roomObjects $ currentRoom gameState)}
      -- Remove the key from the inventory
      updatedInventory = filter (\obj -> objectName obj /= keyName) (inventory gameState)
      -- Create newAllRooms, where the InitialRoom is replaced with the updatedRoom
      currentRoomObj = currentRoom gameState
   in (Just $ gameState {currentRoom = updatedRoom, allRooms = Map.insert (roomName currentRoomObj) updatedRoom (allRooms gameState), inventory = updatedInventory}, Just (doorName ++ " succesfully unlocked.\n"))

sawOffTableLeg :: GameState -> (Maybe GameState, Maybe String)
sawOffTableLeg gameState =
  -- Find remove table and add wooden table leg to the room
  let updatedRoom = (currentRoom gameState) {roomObjects = wooden_table_leg : filter (\o -> objectName o /= "table") (roomObjects $ currentRoom gameState)}
      -- Remove the saw from the inventory and add the leg
      updatedInventory = filter (\obj -> objectName obj /= "hand_saw") (inventory gameState)
   in -- Add updatedRoom to allRooms
      ( Just $ gameState {currentRoom = updatedRoom, allRooms = Map.insert (roomName updatedRoom) updatedRoom (allRooms gameState), inventory = updatedInventory},
        Just
          "You manage to sever the loose leg with your saw. \n\
          \The second the leg comes off the saw breaks.\n A *wooden_table_leg* lies on the floor"
      )

set_leg_on_fire :: GameState -> (Maybe GameState, Maybe String)
set_leg_on_fire gameState =
  let updatedInventory = filter (\obj -> objectName obj /= "wooden_table_leg") $ inventory gameState
      updatedInventory' = makeshift_torch : updatedInventory
   in ( Just $
          gameState
            { inventory = updatedInventory',
              allRooms = allRooms gameState,
              currentRoom = currentRoom gameState
            },
        Just "You fashion the table leg into a makeshift torch."
      )

set_alien_on_fire :: GameState -> (Maybe GameState, Maybe String)
set_alien_on_fire gameState =
  let updatedRoom =
        (currentRoom gameState)
          { roomObjects =
              filter
                (\o -> objectName o /= "alien_mass")
                (roomObjects $ currentRoom gameState)
          }

      updatedInventory =
        filter
          (\o -> objectName o /= "makeshift_torch")
          (inventory gameState)
      updatedRoom' = addExit South "escape_pods" updatedRoom
   in ( Just $
          gameState
            { currentRoom = updatedRoom',
              inventory = updatedInventory,
              allRooms = Map.insert (roomName updatedRoom') updatedRoom' (allRooms gameState)
            },
        Just "The alien mass burns away, revealing a path South to the escape pods!"
      )

fixConsole :: GameState -> (Maybe GameState, Maybe String)
fixConsole gameState =
  let updatedRoom =
        (currentRoom gameState)
          { roomObjects =
              filter (\o -> objectName o /= "broken_console") $
                roomObjects $ currentRoom gameState
          }
      updatedRoom' =
        updatedRoom
          { roomObjects = escape_pod_launch_console : roomObjects updatedRoom
          }
      updatedInventory =
        filter (\o -> objectName o /= "electrical_tools") $
          inventory gameState
   in ( Just $
          gameState
            { currentRoom = updatedRoom',
              allRooms = Map.insert (roomName updatedRoom') updatedRoom' (allRooms gameState),
              inventory = updatedInventory
            },
        Just "You manage to fix the console and use it to lower down the remaining escape pod. You can now access the *escape_pod_launch_console* inside the pod and get out of here."
      )

suckAlienMass :: GameState -> (Maybe GameState, Maybe String)
suckAlienMass gameState =
  let updatedRoom =
        (currentRoom gameState)
          { roomObjects =
              filter
                (\o -> objectName o /= "alien_mass")
                (roomObjects $ currentRoom gameState)
          }

      updatedInventory =
        filter
          (\o -> objectName o /= "metal_statue")
          (inventory gameState)
      updatedRoom' = addExit South "escape_pods" updatedRoom
   in ( Just $
          gameState
            { currentRoom = updatedRoom',
              inventory = updatedInventory,
              allRooms = Map.insert (roomName updatedRoom') updatedRoom' (allRooms gameState)
            },
        Just
          "You hurl the statue at the window, breaking it \n The air begins to get sucked out the room at an incredible speed.\n\
          \You grab onto the nearest pipe. All loose objects in the room fly out of the window\
          \and the alien mass gets sucked out with them, leaving the path south clear. \n\
          \ the emergency force field kicks in and seals the hole before you lose consciousness"
      )

finishGame :: GameState -> (Maybe GameState, Maybe String)
finishGame gameState =
  ( Just gameState,
    Just
      "You punch in the code. The door to the pod closes behind you and you hear a robotic voice come from the console: \n\
      \Voice: Launch sequence initiated. Please take a seat and fasten your seatbelts. \n\
      \You sit down and hope for the best. After a 20 second countdown the pod begins to shake and propels itself out of the station. \n\
      \ You made it. As you're leaving the station you see the ship is covered in a moving blanket of black material. \n\
      \You live to tell the tale. You try contacting the closest colony and explain the situation. You get permission for emergency landing. \n\
      \Congratulations! You managed to escape the station!"
  )

-- Create a hammerCabinet function that takes a GameState and sets the value of "openable" to True for the Supply_Cabinet, removes the hammer and prints a message
hammerCabinet :: GameState -> (Maybe GameState, Maybe String)
hammerCabinet gameState =
  let updatedRoom = (currentRoom gameState) {roomObjects = map (\obj -> if objectName obj == "Supply_Cabinet" then obj {objectValues = Map.insert "openable" True (objectValues obj)} else obj) (roomObjects $ currentRoom gameState)}
      -- Remove the hammer from the inventory
      updatedInventory = filter (\obj -> objectName obj /= "Hammer") (inventory gameState)
      -- Create newAllRooms, where the InitialRoom is replaced with the updatedRoom
      currentRoomObj = currentRoom gameState
   in (Just $ gameState {currentRoom = updatedRoom, allRooms = Map.insert (roomName currentRoomObj) updatedRoom (allRooms gameState), inventory = updatedInventory}, Just "\nAh yes, brute force. Always a good solution. \nAfter smashing the cabinet open with a hammer, you can look for anything useful that you can find inside. \n")

putOutFire :: GameState -> (Maybe GameState, Maybe String)
putOutFire gameState =
  let updatedRoom = (currentRoom gameState) {roomObjects = map (\obj -> if objectName obj == "Electric_Box" then obj {objectDescription = "The Electric_Box is not burning anymore"} else obj) (roomObjects $ currentRoom gameState)}
      -- Remove the hammer from the inventory
      newRoomObjects = southCorridorExitDoor : roomObjects updatedRoom
      newRoomObjects2 = woundedEngineeringChief : newRoomObjects
      newRoomObjects3 = cantineEntranceDoor : newRoomObjects2
      updatedInventory = filter (\obj -> objectName obj /= "Thick_Blanket") (inventory gameState)

      updatedRoom2 = updatedRoom {roomObjects = newRoomObjects3}
      -- Create newAllRooms, where the InitialRoom is replaced with the updatedRoom
      currentRoomObj = currentRoom gameState
   in (Just $ gameState {currentRoom = updatedRoom2, allRooms = Map.insert (roomName currentRoomObj) updatedRoom2 (allRooms gameState), inventory = updatedInventory}, Just "You put out the fire with the blanket. \nThe electric box is out. You can see the rest of the room clearly now. \n")

-- When using the transaltor, change the description of the wounded engineering chief to the translated one
translateChief :: GameState -> (Maybe GameState, Maybe String)
translateChief gameState =
  let updatedRoom =
        (currentRoom gameState)
          { roomObjects =
              map
                ( \obj ->
                    if objectName obj
                      == "Wounded_Engineering_Chief"
                      then
                        obj
                          { objectDescription =
                              "You: Hey, chief, are you okay? What happened? \n\
                              \Qaux'ods: I'm not sure. I was in the cantine, when the ship started shaking. I ran out to see what's going on and I saw a bright flash of light. \n\
                              \ The captain told me through the radio (plot_element), but as soon as he started explaining, the radio went silent. \n\
                              \ I ran to the main corridor to see if I can help anyone, but I was hit by a piece of debris. I'm not sure how long I can hold on. \n\
                              \ You: I'm sure I can help you somehow! We can get out of here togheter! \n\
                              \ Qaux'ods: No, I'm afraid it's too late for me. You need to go on and survive. The ship took a heavy blow, it won't hold on for long. \n\
                              \ Take my access card, it will open up my office, it will help you get to the escape pod. Get there and escape. \n\
                              \ You: Thank you, chief. I will never forget what you've done for me. \n\
                              \ Qaux'ods: It's nothing but my duty. Now go on, I need to rest. \n"
                          }
                      else obj
                )
                (roomObjects $ currentRoom gameState)
          }
      -- Remove the translator from the inventory
      updatedInventory = filter (\obj -> objectName obj /= "Universal_Speech_Translator") (inventory gameState)
      updatedInventory2 = engineeringChiefAccessCard : updatedInventory
      -- Create newAllRooms, where the InitialRoom is replaced with the updatedRoom
      currentRoomObj = currentRoom gameState
   in (Just $ gameState {currentRoom = updatedRoom, allRooms = Map.insert (roomName currentRoomObj) updatedRoom (allRooms gameState), inventory = updatedInventory2}, Just "You used the translator on the wounded engineering chief. \n")

lowerBridge :: GameState -> (Maybe GameState, Maybe String)
lowerBridge gameState =
  let updatedRoom =
        (currentRoom gameState)
          { roomName = "Engine Room",
            roomDescription = "After the bridge felt down, it left *bridge_gap* and opened entrance to the *nearby_vent*, that looks like it could be crawled into.",
            roomExits =
              Map.fromList [(West, "Engine Room Vent"), (North, "Main Corridor")],
            roomObjects = [bridgeGap]
          }
      currentRoomObj = currentRoom gameState
   in (Just $ gameState {currentRoom = updatedRoom, allRooms = Map.insert (roomName currentRoomObj) updatedRoom (allRooms gameState)}, Just "The key seemes to fit perfectly, you hear a loud noise, and the bridge to the (plot_element_ID1) starts lowering down...\nIt is making a loud noise, engine room must have really taken a lot of damage.\nSNAP! The bridge broke down, and fell to the bottom. On its way it made a hole in nearby vent.")

flashOnCrate :: GameState -> (Maybe GameState, Maybe String)
flashOnCrate gameState = (Just gameState, Just "Fingerprints can only be seen on 9 and 1 butons. \n")

openCrate :: GameState -> (Maybe GameState, Maybe String)
openCrate gameState =
  let currRoom = currentRoom gameState
      updatedRoom =
        currRoom
          { roomDescription = roomDescription currRoom,
            roomName = roomName currRoom,
            roomExits = roomExits currRoom,
            roomObjects = filter (\obj -> objectName obj /= "Locked_Crate") (roomObjects currRoom) ++ [spaceSuitHelmet, spaceSuitTrousers, spaceSuitJacket]
          }
   in ( Just $
          gameState
            { currentRoom = updatedRoom,
              allRooms = Map.insert (roomName currRoom) updatedRoom (allRooms gameState),
              inventory = inventory gameState
            },
        Just "Nice! The code worked! *Space_Suit_Trousers* and *Space_Suit_Helmet* wre inside."
      )

useLadder :: GameState -> (Maybe GameState, Maybe String)
useLadder gameState =
  let currRoom = currentRoom gameState
      updatedRoom =
        currRoom
          { roomDescription = roomDescription currRoom,
            roomName = roomName currRoom,
            roomExits = roomExits currRoom,
            roomObjects = filter (\obj -> objectName obj /= "Bridge_Gap") (roomObjects currRoom)
          }
      updatedRoom2 = addExit South "workshop" updatedRoom
      currentRoomObj = currentRoom gameState
   in ( Just $
          gameState
            { currentRoom = updatedRoom2,
              allRooms = Map.insert (roomName currentRoomObj) updatedRoom2 (allRooms gameState),
              inventory = inventory gameState
            },
        Just "You put the ladder on the gap, and now you can cross it."
      )
