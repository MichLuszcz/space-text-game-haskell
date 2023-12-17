module UseHandling where

import Data.List
import qualified Data.Map.Strict as Map
import DataTypes
import Distribution.Compat.Lens (use)
import GameObjects
import ObjectManagment

-- Function to handle the use command like openDoor from DoorHandling.hs
useItem :: String -> String -> GameState -> (Maybe GameState, Maybe String)
useItem itemName targetName gameState =
  case findObject itemName (inventory gameState) of
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
          _ -> (Nothing, Just "Item not found")
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
  let updatedRoom = (currentRoom gameState) {roomObjects = wooden_table_leg: filter (\o -> objectName o /= "table") (roomObjects $ currentRoom gameState)}
      -- Remove the saw from the inventory and add the leg
      updatedInventory = filter (\obj -> objectName obj /= "hand_saw") (inventory gameState)
   in -- Add updatedRoom to allRooms
      (Just $ gameState {currentRoom = updatedRoom, allRooms = Map.insert (roomName updatedRoom) updatedRoom (allRooms gameState), inventory = updatedInventory}, 
      Just "You manage to sever the loose leg with your saw. \n\
      \The second the leg comes off the saw breaks.\n A *wooden_table_leg* lies on the floor")


set_leg_on_fire :: GameState -> (Maybe GameState, Maybe String)
set_leg_on_fire gameState =
  let updatedInventory = filter (\obj -> objectName obj /= "wooden_table_leg") $ inventory gameState
      updatedInventory' = makeshift_torch : updatedInventory
   in (Just $ gameState {inventory = updatedInventory',
                         allRooms = allRooms gameState,
                         currentRoom = currentRoom gameState},
       Just "You fashion the table leg into a makeshift torch.")



set_alien_on_fire :: GameState -> (Maybe GameState, Maybe String)
set_alien_on_fire gameState =
  let updatedRoom = (currentRoom gameState) {roomObjects = filter (\o -> objectName o /= "alien_mass") 
                                           (roomObjects $ currentRoom gameState)}

      updatedInventory = filter (\o -> objectName o /= "makeshift_torch")
                             (inventory gameState)
      updatedRoom' = addExit South "escape_pods" updatedRoom
   in (Just $ gameState {currentRoom = updatedRoom',
                         inventory = updatedInventory,
                         allRooms = Map.insert (roomName updatedRoom') updatedRoom' (allRooms gameState)},
       Just "The alien mass burns away, revealing a path to the escape pods!")


fixConsole :: GameState -> (Maybe GameState, Maybe String)
fixConsole gameState =
  let updatedRoom = (currentRoom gameState) {
           roomObjects = filter (\o -> objectName o /= "broken_console") $
                            roomObjects $ currentRoom gameState 
                            
       }
      updatedRoom' = updatedRoom {
           roomObjects = escape_pod_launch_console : roomObjects updatedRoom  
       } 
      updatedInventory = filter (\o -> objectName o /= "electrical_tools") $
                           inventory gameState
   in (Just $ gameState {currentRoom = updatedRoom',  
                        allRooms = Map.insert (roomName updatedRoom') updatedRoom' (allRooms gameState),
                         inventory = updatedInventory},
       Just "You manage to fix the console and use it to lower down the remaining escape pod. You can now access the *escape_pod_launch_console* inside the pod and get out of here.")

suckAlienMass :: GameState -> (Maybe GameState, Maybe String)
suckAlienMass gameState =
  let updatedRoom = (currentRoom gameState) {roomObjects = filter (\o -> objectName o /= "alien_mass") 
                                           (roomObjects $ currentRoom gameState)}

      updatedInventory = filter (\o -> objectName o /= "metal_statue")
                             (inventory gameState)
      updatedRoom' = addExit South "escape_pods" updatedRoom
   in (Just $ gameState {currentRoom = updatedRoom',
                         inventory = updatedInventory,
                         allRooms = Map.insert (roomName updatedRoom') updatedRoom' (allRooms gameState)},
       Just "You hurl the statue at the window, breaking it \n The air begins to get sucked out the room at an incredible speed.\n\
       \You grab onto the nearest pipe. All loose objects in the room fly out of the window\
        \and the alien mass gets sucked out with them, leaving the path south clear. \n\
        \ the emergency force field kicks in and seals the hole before you lose consciousness")


finishGame :: GameState -> (Maybe GameState, Maybe String)
finishGame gameState = (Just gameState,
       Just "You punch in the code. The door to the pod closes behind you and you hear a robotic voice come from the console: \n\
       \Voice: Launch sequence initiated. Please take a seat and fasten your seatbelts. \n\
       \You sit down and hope for the best. After a 20 second countdown the pod begins to shake and propels itself out of the station. \n\
       \ You made it. As you're leaving the station you see the ship is covered in a moving blanket of black material. \n\
       \You live to tell the tale. You try contacting the closest colony and explain the situation. You get permission for emergency landing. \n\
       \Congratulations! You managed to escape the station!")
