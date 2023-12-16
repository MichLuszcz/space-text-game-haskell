module DoorHandling where

import Data.Map.Strict qualified as Map
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
          _ -> (Nothing, Just "Door not found")
      Just False -> (Nothing, Just "\n This door cannot be opened.")
    Nothing -> (Nothing, Just "\n Door not found.")

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
      updatedWorkshop = addExit North "engineering_chief_office_door" (findRoom "workshop" rooms)
      updatedAllRooms = Map.insert (roomName updatedWorkshop) updatedWorkshop (allRooms gameState)
   in (Just $ gameState {allRooms = updatedAllRooms, currentRoom = updatedWorkshop},
       Just "Engineering Chief's office door opened.")

  where
    rooms = Map.elems (allRooms gameState)
