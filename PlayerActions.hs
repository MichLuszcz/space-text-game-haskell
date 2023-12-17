module PlayerActions where

import Data.List
import  qualified Data.Map.Strict as Map
-- Module imports
import DataTypes
import GameObjects
import ObjectManagment
import UtilityFunctions

-- Function to move the player to a new room
move :: Direction -> GameState -> Maybe GameState
move direction gameState = do
  nextRoomName <- Map.lookup direction (roomExits $ currentRoom gameState)
  case findRoom nextRoomName (Map.elems $ allRooms gameState) of
    nextRoom -> return $ gameState {currentRoom = nextRoom}

-- Function to pick up an object in the current room
pickUp :: String -> GameState -> (Maybe GameState, Maybe String)
pickUp targetName gameState =
  case findObject targetName (roomObjects $ currentRoom gameState) of
    Just obj -> case Map.lookup "pickable" (objectValues obj) of
      Just True ->
        let newInventory = obj : inventory gameState
            newRoomObjects = filter (\o -> targetName /= objectName o) (roomObjects $ currentRoom gameState)
            updatedRoom = (currentRoom gameState) {roomObjects = newRoomObjects}
            currentRoomObj = currentRoom gameState
         in (Just $ gameState {inventory = newInventory, currentRoom = updatedRoom, allRooms = Map.insert (roomName currentRoomObj) updatedRoom (allRooms gameState)}, Just "\nPickup successful \n")
      _ -> (Nothing, Just "\nObject not pickable\n")
    Nothing -> (Nothing, Just "\nObject not found \n")

-- Function to inspect an object in the current room
inspect :: String -> GameState -> Maybe String
inspect targetName gameState = do
  obj <- findObject targetName (roomObjects $ currentRoom gameState)
  return $ "\nYou inspect the " ++ targetName ++ ".\n\n" ++ objectDescription obj ++ "\n"

-- Function for cable connecting minigame - if combination is correect (yrb) then the box opens and cyberKey is added to the room
connectCables :: String -> String -> String -> GameState -> (Maybe GameState, Maybe String)
connectCables cab1 cab2 cab3 gameState =
  case cab1 ++ cab2 ++ cab3 of
    "yrb" ->
      let newRoomObjects = cyberKey : roomObjects (currentRoom gameState)

          -- Remove the box from the room
          newRoomObjectsWithoutBox = filter (\o -> "Locked_Safety_Box" /= objectName o) newRoomObjects
          updatedRoom = (currentRoom gameState) {roomObjects = newRoomObjectsWithoutBox}
          currentRoomObj = currentRoom gameState
       in (Just $ gameState {currentRoom = updatedRoom, allRooms = Map.insert (roomName currentRoomObj) updatedRoom (allRooms gameState)}, Just "\nYou hear a click and the box opens. Something fell out of the box on the floor. \n")
    _ -> (Nothing, Just "\nYou hear a click, but the box doesn't open. \n")

-- Function to look at exits in the current room
checkExits :: GameState -> IO ()
checkExits gameState = do
  let exits = roomExits (currentRoom gameState)
  putStrLn "\nAvailable directions:"
  mapM_ (\(dir, room) -> putStrLn $ show dir ++ " -> " ++ room) (Map.toList exits)
  printEmptyLine