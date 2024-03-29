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
          "toolbox" -> openToolbox gameState
          "closed_computer" -> openComputer gameState
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


openToolbox :: GameState -> (Maybe GameState, Maybe String)
openToolbox gameState =
  let newRoomObjects = filter (\o -> objectName o /= "toolbox") (roomObjects $ currentRoom gameState)
      newRoomObjectsWithTools = hand_saw : electrical_tools : newRoomObjects  
      updatedRoom = (currentRoom gameState) {roomObjects = newRoomObjectsWithTools}
      updatedAllRooms = Map.insert (roomName updatedRoom) updatedRoom (allRooms gameState)
   in (Just $ gameState {allRooms = updatedAllRooms, currentRoom = updatedRoom},
      Just "Toolbox opened, you see a *hand_saw* and *electrical_tools*. The saw seems to be covered in rust, but it might be good for a single use.")


openComputer :: GameState -> (Maybe GameState, Maybe String)
openComputer gameState =
  let updatedRoom' = (currentRoom gameState) {
           roomObjects = filter (\o -> objectName o /= "closed_computer") $
                            roomObjects $ currentRoom gameState
       }
      updatedRoom =  updatedRoom' {
        roomObjects = opened_computer : roomObjects updatedRoom'
        
      }
      updatedInventory = code_1867 : inventory gameState
      updatedAllRooms = Map.insert (roomName updatedRoom) updatedRoom (allRooms gameState)
   in (Just $ gameState {currentRoom = updatedRoom, allRooms = updatedAllRooms, inventory = updatedInventory},
       Just "You open the closed_computer sitting on the desk. You find an open email titled ESCAPE POD CODE UPDATE: \n\
\ Hi, Qaux'ods, please remember about the annual escape pod tests. We've changed all the codes to *1867* for this week to make the process easier. \n\
\ Please have the report done by next week. Cheers.\n You keep the code in mind for later")
openSupplyCabinet :: GameState -> (Maybe GameState, Maybe String)
openSupplyCabinet gameState =
  -- Create an updatedRoom where the locker is removed from the roomObjects list, and an access card is added to the roomObjects list
  let newRoomObjects = filter (\o -> "Supply_Cabinet" /= objectName o) (roomObjects $ currentRoom gameState)
      -- Add AcccessCard to newRoomObjects
      newRoomObjectsWithTranslator = universalSpeechTranslator : newRoomObjects
      updatedRoom = (currentRoom gameState) {roomObjects = newRoomObjectsWithTranslator}
      updatedAllRooms = Map.insert (roomName updatedRoom) updatedRoom (allRooms gameState)
   in (Just $ gameState {allRooms = updatedAllRooms, currentRoom = updatedRoom}, Just "\nSupply Cabinet opened.\n")
