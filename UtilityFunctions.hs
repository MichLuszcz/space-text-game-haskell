module UtilityFunctions where

import Data.List
import Data.Map (elems, toList)
import DataTypes

-- Print empty line
printEmptyLine :: IO ()
printEmptyLine = putStrLn ""

-- Print separator (empty line - dashed line - empty line)
printSeparator :: IO ()
printSeparator = do
  printEmptyLine
  putStrLn "----------------------------------------"
  printEmptyLine

-- Print instructions
printInstructions :: IO ()
printInstructions = do
  putStrLn "Available commands are:"
  putStrLn ""
  putStrLn "move <direction> -- to move to a different room."
  putStrLn "pick <item>      -- to pick up an item in the current room."
  putStrLn "inspect <item>   -- to inspect an item in the current room."
  putStrLn "look             -- to look around the current room."
  putStrLn "check inventory  -- to check your inventory."
  putStrLn "quit             -- to end the game and quit."
  putStrLn "instructions     -- to see these instructions."
  putStrLn ""

-- Function to list names of items in the player's inventory
listInventory :: GameState -> String
listInventory gameState =
  if null inventoryList
    then "Your inventory is empty. \n"
    else "Inventory:\n" ++ intercalate "\n" (map (\obj -> "  - " ++ objectName obj) inventoryList) ++ "\n"
  where
    inventoryList = inventory gameState

-- List all Room names from GameState (allRooms)
listRooms :: GameState -> String
listRooms gameState = intercalate "\n" (map roomName (elems (allRooms gameState)))