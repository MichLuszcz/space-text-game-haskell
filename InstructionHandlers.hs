module InstructionHandlers
  ( handleMove,
    handlePick,
    handleOpen,
    handleInspect,
    handleLook,
    invalidInput,
    gameLoop,
  )
where

import DataTypes
import DoorHandling
import GameObjects
import PlayerActions
import UseHandling
import UtilityFunctions

handleMove :: Direction -> GameState -> IO ()
handleMove direction gameState = case move direction gameState of
  Just newState -> gameLoop newState
  Nothing -> do
    putStrLn "You can't go that way."
    gameLoop gameState

handlePick :: String -> GameState -> IO ()
handlePick itemName gameState = do
  let (newState, resultMsg) = pickUp itemName gameState
  case resultMsg of
    Just msg -> do
      putStrLn msg
      case newState of
        Just state -> gameLoop state
        Nothing -> gameLoop gameState
    Nothing -> return ()

handleOpen :: String -> GameState -> IO ()
handleOpen itemName gameState = do
  let (newState, resultMsg) = openDoor itemName gameState
  case resultMsg of
    Just msg -> do
      putStrLn msg
      case newState of
        Just state -> gameLoop state
        Nothing -> gameLoop gameState
    Nothing -> return ()

handleUse :: String -> String -> GameState -> IO ()
handleUse itemName targetName gameState = do
  let (newState, resultMsg) = useItem itemName targetName gameState
  case resultMsg of
    Just msg -> do
      putStrLn msg
      case newState of
        Just state -> gameLoop state
        Nothing -> gameLoop gameState
    Nothing -> return ()

handleInspect :: String -> GameState -> IO ()
handleInspect itemName gameState = case inspect itemName gameState of
  Just result -> do
    putStrLn result
    gameLoop gameState
  Nothing -> do
    putStrLn $ "There is no " ++ itemName ++ " here."
    gameLoop gameState

handleLook :: GameState -> IO ()
handleLook gameState = do
  putStrLn "You look around and see:"
  mapM_ (\obj -> putStrLn $ "  - " ++ objectName obj) (roomObjects $ currentRoom gameState)
  gameLoop gameState

invalidInput :: GameState -> IO ()
invalidInput gameState = do
  putStrLn "Invalid command. Try again."
  gameLoop gameState

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
      "N" -> handleMove North gameState
      "S" -> handleMove South gameState
      "W" -> handleMove West gameState
      "E" -> handleMove East gameState
      _ -> invalidInput gameState
    ["pick", itemName] -> handlePick itemName gameState
    ["inspect", itemName] -> handleInspect itemName gameState
    ["open", itemName] -> handleOpen itemName gameState
    ["use", itemName, "on", targetName] -> handleUse itemName targetName gameState
    ["look"] -> handleLook gameState
    ["check", "inventory"] -> do
      putStrLn $ listInventory gameState
      gameLoop gameState
    ["check", "exits"] -> do
      checkExits gameState
      gameLoop gameState
    _ -> invalidInput gameState