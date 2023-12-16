module UseHandling where

import Data.List
import Data.Map.Strict qualified as Map
import DataTypes
import Distribution.Compat.Lens (use)
import ObjectManagment

-- Function to handle the use command like openDoor from DoorHandling.hs
useItem :: String -> String -> GameState -> (Maybe GameState, Maybe String)
useItem itemName targetName gameState =
  case findObject itemName (inventory gameState) of
    Just obj -> case Map.lookup "usable" (objectValues obj) of
      Just True ->
        -- Make a case of from the itemName and targetName, and the cases should call separate functions like keyUnlockNorthDoor
        case (itemName, targetName) of
          ("key", "North-Door") -> keyUnlockNorthDoor gameState
          ("hand_saw", "table") -> sawOffTableLeg gameState
          _ -> (Nothing, Just "Item not found")
      Just False -> (Nothing, Just "This item cannot be used.")
    Nothing -> (Nothing, Just "Item not found")

-- Use key on North-Door to set the value of "openable" to True and remove the key from the inventory
keyUnlockNorthDoor :: GameState -> (Maybe GameState, Maybe String)
keyUnlockNorthDoor gameState =
  -- Find Object "North-Door" in the current room and set the value of "openable" to True
  let updatedRoom = (currentRoom gameState) {roomObjects = map (\obj -> if objectName obj == "North-Door" then obj {objectValues = Map.insert "openable" True (objectValues obj)} else obj) (roomObjects $ currentRoom gameState)}
      -- Remove the key from the inventory
      updatedInventory = filter (\obj -> objectName obj /= "key") (inventory gameState)
   in -- Add updatedRoom to allRooms
      (Just $ gameState {currentRoom = updatedRoom, allRooms = Map.insert (roomName updatedRoom) updatedRoom (allRooms gameState), inventory = updatedInventory}, Just "North-Door unlocked.\n")


sawOffTableLeg :: gameState -> (Maybe GameState, Maybe String)
sawOffTableLeg gameState =
  -- Find remove table and add wooden table leg to the room
  let updatedRoom = (currentRoom gameState) {roomObjects = wooden_table_leg: filter (\o -> targetName /= "table") (roomObjects $ currentRoom gameState)}
      -- Remove the saw from the inventory and add the leg
      updatedInventory = filter (\obj -> objectName obj /= "hand_saw") (inventory gameState)
   in -- Add updatedRoom to allRooms
      (Just $ gameState {currentRoom = updatedRoom, allRooms = Map.insert (roomName updatedRoom) updatedRoom (allRooms gameState), inventory = updatedInventory}, 
      Just "You manage to sever the loose leg with your saw. 
      The second the leg comes off the saw breaks.\n A *wooden_table_leg* lies on the floor")
