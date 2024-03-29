module DoorHandling where

import qualified Data.Map.Strict as Map
import DataTypes
import ObjectManagment

-- Handle opening a door
openDoor :: String -> GameState -> (Maybe GameState, Maybe String)
openDoor doorToOpen gameState =
  case findObject doorToOpen (roomObjects $ currentRoom gameState) of
    Just obj -> case Map.lookup "openable" (objectValues obj) of
      Just True ->
        -- Make a case of from the doorName, and the cases should call separate functions like openNorthDoor, openSouthDoor, etc.
        case doorToOpen of
          "Security_Door" -> openSecurityDoor gameState
          "engineering_chief_office_door" -> open_engineering_chief_office_door gameState
          "Cantine_Entrance_Door" -> openCantineEntranceDoor gameState
          "South_Corridor_Exit_Door" -> openSouthCorridorExitDoor gameState
          "Void_Entrance" -> openVoidEntrance gameState
          _ -> (Nothing, Just "Door not found")
      Just False -> (Nothing, Just "\nThis door cannot be opened.")
    Nothing -> (Nothing, Just "\nDoor not found.")

openSecurityDoor :: GameState -> (Maybe GameState, Maybe String)
openSecurityDoor gameState =
  let updatedCrewRoom = addExit South "Main Corridor" (findRoom "Crew Bedroom" rooms)
      updatedMainRoom = addExit North "Crew Bedroom" (findRoom "Main Corridor" rooms)
      updatedAllRooms = Map.insert (roomName updatedCrewRoom) updatedCrewRoom (allRooms gameState)
      updatedAllRoomsSec = Map.insert (roomName updatedMainRoom) updatedMainRoom updatedAllRooms
   in (Just $ gameState {allRooms = updatedAllRoomsSec, currentRoom = updatedCrewRoom}, Just "\nSecurity Door to the Main Corridor opened.\n")
  where
    rooms = Map.elems (allRooms gameState)


open_engineering_chief_office_door :: GameState -> (Maybe GameState, Maybe String)  
open_engineering_chief_office_door gameState =
  let
      updatedWorkshop = addExit North "engineering_chief_office" (findRoom "workshop" rooms)
      updatedAllRooms = Map.insert (roomName updatedWorkshop) updatedWorkshop (allRooms gameState)
   in (Just $ gameState {allRooms = updatedAllRooms, currentRoom = updatedWorkshop},
       Just "Engineering Chief's office door opened.")
  where
    rooms = Map.elems (allRooms gameState)

openCantineEntranceDoor :: GameState -> (Maybe GameState, Maybe String)
openCantineEntranceDoor gameState =
  let updatedCantineRoom = addExit West "Main Corridor" (findRoom "Cantine" rooms)
      updatedMainRoom = addExit East "Cantine" (findRoom "Main Corridor" rooms)
      changedDescMainRoom = updatedMainRoom {roomDescription = "You are in the Main Corridor."}
      updatedAllRooms = Map.insert (roomName updatedCantineRoom) updatedCantineRoom (allRooms gameState)
      updatedAllRoomsSec = Map.insert (roomName changedDescMainRoom) changedDescMainRoom updatedAllRooms
   in (Just $ gameState {allRooms = updatedAllRoomsSec, currentRoom = updatedMainRoom}, Just "\nCantine Entrance Door to the Cantine opened.\n")
  where
    rooms = Map.elems (allRooms gameState)

openSouthCorridorExitDoor :: GameState -> (Maybe GameState, Maybe String)
openSouthCorridorExitDoor gameState =
  let updatedEngineRoom = addExit North "Main Corridor" (findRoom "Engine Room" rooms)
      updatedMainRoom = addExit South "Engine Room" (findRoom "Main Corridor" rooms)
      updatedAllRooms = Map.insert (roomName updatedEngineRoom) updatedEngineRoom (allRooms gameState)
      updatedAllRoomsSec = Map.insert (roomName updatedMainRoom) updatedMainRoom updatedAllRooms
   in (Just $ gameState {allRooms = updatedAllRoomsSec, currentRoom = updatedMainRoom}, Just "\nSouth Corridor Exit Door to the Engine Room opened.\n")
  where
    rooms = Map.elems (allRooms gameState)

openVoidEntrance :: GameState -> (Maybe GameState, Maybe String)
openVoidEntrance gameState =
  if "Space_Suit" `elem` map objectName (inventory gameState)
    then
      let updatedVoidRoom = addExit West "Crew Bedroom Vent" (findRoom "The Void" rooms)
          updatedMainRoom = addExit East "The Void" (findRoom "Crew Bedroom Vent" rooms)
          updatedAllRooms = Map.insert (roomName updatedVoidRoom) updatedVoidRoom (allRooms gameState)
          updatedAllRoomsSec = Map.insert (roomName updatedMainRoom) updatedMainRoom updatedAllRooms
       in (Just $ gameState {allRooms = updatedAllRoomsSec, currentRoom = updatedMainRoom}, Just "\nVoid Entrance Door to the Crew Bedroom Vent opened.\n")
    else (Nothing, Just "You died!")
  where
    rooms = Map.elems (allRooms gameState)
