module OpenHandling where

import Data.List
import qualified Data.Map.Strict as Map
import DataTypes
import GameObjects
import ObjectManagment

openObject :: String -> GameState -> (Maybe GameState, Maybe String)
openObject objectToOpen gameState =
  case findObject objectToOpen (roomObjects $ currentRoom gameState) of
    Just obj -> case Map.lookup "openable" (objectValues obj) of
      Just True ->
        -- Make a case of from the objectName, and the cases should call separate functions like openNorthDoor, openSouthDoor, etc.
        case objectToOpen of
          "Desk" -> openDesk gameState
          "Locker" -> openLocker gameState
          "Supply_Cabinet" -> openSupplyCabinet gameState
          _ -> (Nothing, Just "Object not found")
      Just False -> (Nothing, Just "\nThis object cannot be opened.")
    Nothing -> (Nothing, Just "\nObject not found.")

openDesk :: GameState -> (Maybe GameState, Maybe String)
openDesk gameState =
  -- Create an updatedRoom where the desk is removed from the roomObjects list, and an access card is added to the roomObjects list
  let newRoomObjects = filter (\o -> "Desk" /= objectName o) (roomObjects $ currentRoom gameState)
      -- Add AcccessCard to newRoomObjects
      newRoomObjectsWithCard = accessCard : newRoomObjects
      updatedRoom = (currentRoom gameState) {roomObjects = newRoomObjectsWithCard}
      updatedAllRooms = Map.insert (roomName updatedRoom) updatedRoom (allRooms gameState)
   in (Just $ gameState {allRooms = updatedAllRooms, currentRoom = updatedRoom}, Just "\nDesk opened.\n")

openLocker :: GameState -> (Maybe GameState, Maybe String)
openLocker gameState =
  -- Create an updatedRoom where the locker is removed from the roomObjects list, and an access card is added to the roomObjects list
  let newRoomObjects = filter (\o -> "Locker" /= objectName o) (roomObjects $ currentRoom gameState)
      -- Add AcccessCard to newRoomObjects
      newRoomObjectsWithCard = hammer : newRoomObjects
      updatedRoom = (currentRoom gameState) {roomObjects = newRoomObjectsWithCard}
      updatedAllRooms = Map.insert (roomName updatedRoom) updatedRoom (allRooms gameState)
   in (Just $ gameState {allRooms = updatedAllRooms, currentRoom = updatedRoom}, Just "\nLocker opened.\n")

openSupplyCabinet :: GameState -> (Maybe GameState, Maybe String)
openSupplyCabinet gameState =
  -- Create an updatedRoom where the locker is removed from the roomObjects list, and an access card is added to the roomObjects list
  let newRoomObjects = filter (\o -> "Supply_Cabinet" /= objectName o) (roomObjects $ currentRoom gameState)
      -- Add AcccessCard to newRoomObjects
      newRoomObjectsWithTranslator = universalSpeechTranslator : newRoomObjects
      updatedRoom = (currentRoom gameState) {roomObjects = newRoomObjectsWithTranslator}
      updatedAllRooms = Map.insert (roomName updatedRoom) updatedRoom (allRooms gameState)
   in (Just $ gameState {allRooms = updatedAllRooms, currentRoom = updatedRoom}, Just "\nSupply Cabinet opened.\n")
