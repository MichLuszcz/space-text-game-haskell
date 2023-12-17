module ObjectManagment where

import Data.List
import qualified Data.Map.Strict  as Map
import DataTypes

-- Function to describe an object
describeObject :: Object -> String
describeObject (Object name desc _) = "\n" ++ "It's a " ++ name ++ ". \n" ++ desc ++ "\n"

-- Function to find an object by name in a list
findObject :: String -> [Object] -> Maybe Object
findObject targetName = find (\obj -> targetName == objectName obj)

-- Function to find Room by name in list from GameState (allRooms)
findRoom :: String -> [Room] -> Room
findRoom targetName rooms = case find (\room -> targetName == roomName room) rooms of
  Just room -> room
  Nothing -> error $ "Room not found: " ++ targetName

-- Function to add an exit to a room
addExit :: Direction -> String -> Room -> Room
addExit direction toRoomName fromRoom =
  fromRoom {roomExits = Map.insert direction toRoomName (roomExits fromRoom)}
