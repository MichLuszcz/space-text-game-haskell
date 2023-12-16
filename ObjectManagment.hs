module ObjectManagment where

import Data.List
import Data.Map.Strict qualified as Map
import DataTypes

-- Function to describe an object
describeObject :: Object -> String
describeObject (Object name desc _) = "It's a " ++ name ++ ". \n" ++ desc

-- Function to find an object by name in a list
findObject :: String -> [Object] -> Maybe Object
findObject targetName = find (\obj -> targetName == objectName obj)

-- Function to find Room by name in list from GameState (allRooms)
findRoom :: String -> [Room] -> Room
findRoom targetName rooms = case find (\room -> targetName == roomName room) rooms of
  Just room -> room
  Nothing -> error $ "Room not found: " ++ targetName

-- Function to add an exit to a room
addExit :: Direction -> Room -> Room -> Room
addExit direction toRoom fromRoom =
  fromRoom {roomExits = Map.insert direction toRoom (roomExits fromRoom)}
