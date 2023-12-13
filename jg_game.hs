{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

import Control.Monad (guard)
import Control.Monad.RWS.Class (MonadState (put))
import Data.List
import Data.Map.Strict qualified as Map
import Text.XHtml (target)

-- Define a data type to represent directions
data Direction = North | South | West | East
  deriving (Show, Eq, Ord)

data Object = Object
  { objectName :: String,
    objectDescription :: String,
    objectValues :: Map.Map String Bool
  }
  deriving (Show)

data Door = Door
  { doorName :: String,
    openable :: Bool,
    paths :: [(Room, Direction, Room)]
  }
  deriving (Show)

-- Define a data type to represent rooms
data Room = Room
  { roomName :: String,
    roomDescription :: String,
    roomObjects :: [Object],
    roomExits :: Map.Map Direction Room,
    doors :: [Door]
  }
  deriving (Show)

-- Define a data type to represent the game state
data GameState = GameState
  { currentRoom :: Room,
    inventory :: [Object],
    allRooms :: Map.Map String Room
  }
  deriving (Show)

-- Function to describe an object
describeObject :: Object -> String
describeObject (Object name desc _) = "It's a " ++ name ++ ". \n" ++ desc

-- Function to find an object by name in a list
findObject :: String -> [Object] -> Maybe Object
findObject targetName = find (\obj -> targetName == objectName obj)

-- Function to find Room by name in list from GameState (allRooms)
findRoom :: String -> [Room] -> Maybe Room
findRoom targetName = find (\room -> targetName == roomName room)

-- Function to find a door by name in a list
findDoor :: String -> [Door] -> Maybe Door
findDoor targetName = find (\door -> targetName == doorName door)

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

-- Print empty line
printEmptyLine :: IO ()
printEmptyLine = putStrLn ""

-- Print separator (empty line - dashed line - empty line)
printSeparator :: IO ()
printSeparator = do
  printEmptyLine
  putStrLn "----------------------------------------"
  printEmptyLine

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
         in (Just $ gameState {inventory = newInventory, currentRoom = (currentRoom gameState) {roomObjects = newRoomObjects}}, Just "Pickup successful")
      _ -> (Nothing, Just "Object not pickable")
    Nothing -> (Nothing, Just "Object not found")

-- Function to inspect an object in the current room
inspect :: String -> GameState -> Maybe String
inspect targetName gameState = do
  obj <- findObject targetName (roomObjects $ currentRoom gameState)
  return $ "You inspect the " ++ targetName ++ ".\n" ++ objectDescription obj

-- Function to open a door and create paths between rooms
openDoor :: String -> Room -> Maybe (Room, String)
openDoor doorName room = do
  door <- findDoor doorName (doors room)
  guard (openable door)
  let updatedDoor = door {openable = False}
  let updatedPaths = paths updatedDoor
  return $ foldr (\(fromRoom, dir, toRoom) -> addPath fromRoom dir toRoom) room updatedPaths

-- Function toadd a path between rooms
addPath :: Room -> Direction -> Room -> Room -> Room
addPath fromRoom dir toRoom room =
  room {roomExits = Map.insert dir toRoom (roomExits fromRoom)}

-- Function to list names of items in the player's inventory
listInventory :: GameState -> String
listInventory gameState =
  if null inventoryList
    then "Your inventory is empty."
    else "Inventory:\n" ++ intercalate "\n" (map (\obj -> "  - " ++ objectName obj) inventoryList)
  where
    inventoryList = inventory gameState

-- Example game initialization
initialRoom :: Room
initialRoom =
  Room
    { roomName = "Dark Room",
      roomDescription = "You find yourself in a dark room.\n",
      roomObjects =
        [ Object "key" "A shiny golden key." (Map.fromList [("pickable", True)]),
          Object "table" "A wooden table." (Map.fromList [("pickable", False)]),
          Object
            "North-Door"
            "It's a door leading out of the room on the north side. It's closed right now, but you can open it."
            (Map.fromList [("pickable", False), ("openable", True)])
        ],
      doors =
        [ Door
            "North-Door"
            True
            [ (initialRoom, North, nextRoom),
              (nextRoom, South, initialRoom)
            ]
        ],
      roomExits = Map.empty
    }

nextRoom :: Room
nextRoom =
  Room
    { roomName = "Next Room",
      roomDescription = "You enter a mysterious room.\n",
      roomObjects =
        [Object "book" "An old dusty tome." (Map.fromList [("pickable", True)])],
      doors = [],
      roomExits = Map.empty
    }

initialState :: GameState
initialState =
  GameState
    { currentRoom = initialRoom,
      inventory = [],
      allRooms = Map.fromList [("Dark Room", initialRoom), ("Next Room", nextRoom)]
    }

-- Example main function to start the game
main :: IO ()
main = do
  putStrLn "Welcome to Haskell Adventure."
  printSeparator
  printInstructions
  gameLoop initialState

-- Game loop to handle player input
gameLoop :: GameState -> IO ()
gameLoop gameState = do
  putStrLn $ roomDescription (currentRoom gameState)
  putStrLn "What do you want to do?"

  command <- getLine
  printEmptyLine

  case words command of
    ["quit"] -> return ()
    ["instructions"] -> do
      printInstructions
      gameLoop gameState
    ["move", dir] -> case dir of
      "N" -> handleMove North
      "S" -> handleMove South
      "W" -> handleMove West
      "E" -> handleMove East
      _ -> invalidInput
    ["pick", itemName] -> handlePick itemName
    ["inspect", itemName] -> handleInspect itemName
    ["open", itemName] -> handleOpen itemName
    ["look"] -> handleLook
    ["check", "inventory"] -> do
      putStrLn $ listInventory gameState
      gameLoop gameState
    _ -> invalidInput
  where
    handleMove direction = case move direction gameState of
      Just newState -> gameLoop newState
      Nothing -> do
        putStrLn "You can't go that way."
        gameLoop gameState

    handlePick itemName = do
      let (newState, resultMsg) = pickUp itemName gameState
      case resultMsg of
        Just msg -> do
          putStrLn msg
          case newState of
            Just state -> gameLoop state
            Nothing -> gameLoop gameState
        Nothing -> return ()

    handleInspect itemName = case inspect itemName gameState of
      Just result -> do
        putStrLn result
        gameLoop gameState
      Nothing -> do
        putStrLn $ "There is no " ++ itemName ++ " here."
        gameLoop gameState

    handleOpen itemName = do
      let (newState, resultMsg) = openDoor itemName gameState
      case resultMsg of
        Just msg -> do
          putStrLn msg
          case newState of
            Just state -> gameLoop state
            Nothing -> gameLoop gameState
        Nothing -> return ()

    handleLook = do
      putStrLn "You look around and see:"
      mapM_ (\obj -> putStrLn $ "  - " ++ objectName obj) (roomObjects $ currentRoom gameState)
      gameLoop gameState

    invalidInput = do
      putStrLn "Invalid command. Try again."
      gameLoop gameState
