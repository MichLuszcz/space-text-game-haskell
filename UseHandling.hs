module UseHandling where

import Data.List
import Data.Map.Strict qualified as Map
import DataTypes
import Distribution.Compat.Lens (use)
import GameObjects (initialRoom)
import ObjectManagment

-- Function to handle the use command like openDoor from DoorHandling.hs
useItem :: String -> String -> GameState -> (Maybe GameState, Maybe String)
useItem itemName targetName gameState =
  case findObject itemName (inventory gameState) of
    Just obj -> case Map.lookup "usable" (objectValues obj) of
      Just True ->
        -- Make a case of from the itemName and targetName, and the cases should call separate functions like keyUnlockNorthDoor
        case (itemName, targetName) of
          ("key", "North-Door") -> keyUnlock itemName targetName gameState
          _ -> (Nothing, Just "Item not found")
      Just False -> (Nothing, Just "This item cannot be used.")
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
