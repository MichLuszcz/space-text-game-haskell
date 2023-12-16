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
          "North-Door" -> openNorthDoor gameState
          _ -> (Nothing, Just "Door not found")
      Just False -> (Nothing, Just "\n This door cannot be opened.")
    Nothing -> (Nothing, Just "\n Door not found.")

openNorthDoor :: GameState -> (Maybe GameState, Maybe String)
openNorthDoor gameState =
  let updatedInitialRoom = addExit North "Next Room" (findRoom "Dark Room" rooms)
      updatedNextRoom = addExit South "Dark Room" (findRoom "Next Room" rooms)
   in (Just $ gameState {allRooms = Map.fromList [(roomName updatedInitialRoom, updatedInitialRoom), (roomName updatedNextRoom, updatedNextRoom)], currentRoom = updatedInitialRoom}, Just "\nNorth Door opened.\n")
  where
    rooms = Map.elems (allRooms gameState)
