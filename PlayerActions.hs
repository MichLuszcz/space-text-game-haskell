module PlayerActions where

import Data.List
import Data.Map.Strict qualified as Map
-- Module imports
import DataTypes
import ObjectManagment
import UtilityFunctions

-- Function to move the player to a new room
move :: Direction -> GameState -> Maybe GameState
move direction gameState = do
  nextRoom <- Map.lookup direction (roomExits $ currentRoom gameState)
  return $ gameState {currentRoom = nextRoom}

-- Function to pick up an object in the current room
pickUp :: String -> GameState -> (Maybe GameState, Maybe String)
pickUp targetName gameState =
  case findObject targetName (roomObjects $ currentRoom gameState) of
    Just obj -> case Map.lookup "pickable" (objectValues obj) of
      Just True ->
        let newInventory = obj : inventory gameState
            newRoomObjects = filter (\o -> targetName /= objectName o) (roomObjects $ currentRoom gameState)
         in (Just $ gameState {inventory = newInventory, currentRoom = (currentRoom gameState) {roomObjects = newRoomObjects}}, Just "\n Pickup successful \n")
      _ -> (Nothing, Just "\n Object not pickable\n ")
    Nothing -> (Nothing, Just "\n Object not found \n")

-- Function to inspect an object in the current room
inspect :: String -> GameState -> Maybe String
inspect targetName gameState = do
  obj <- findObject targetName (roomObjects $ currentRoom gameState)
  return $ "\n You inspect the " ++ targetName ++ ".\n" ++ objectDescription obj ++ "\n"

-- Function to look at exits in the current room
checkExits :: GameState -> IO ()
checkExits gameState = do
  let exits = roomExits (currentRoom gameState)
  putStrLn "\n Available directions:"
  mapM_ (\(dir, room) -> putStrLn $ show dir ++ " -> " ++ roomName room) (Map.toList exits)
  printEmptyLine