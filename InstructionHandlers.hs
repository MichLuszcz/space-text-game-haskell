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

import qualified Data.Map.Strict as Map
import DataTypes
import DoorHandling
import GHC.IO.Exception (ExitCode (ExitSuccess))
import GameObjects
import ObjectManagment
import OpenHandling
import PlayerActions
import System.Directory.Internal.Prelude (exitFailure)
import System.Exit (exitSuccess)
import UseHandling
import UtilityFunctions

handleMove :: Direction -> GameState -> IO ()
handleMove direction gameState = case move direction gameState of
  Just newState -> do
    printEmptyLine
    putStrLn $ roomDescription (currentRoom newState)
    gameLoop newState
  Nothing -> do
    printEmptyLine
    putStrLn "You can't go that way."
    printEmptyLine
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
-- Case of if itemName has the flag door set to true, then call openDoor, otherwise call open
handleOpen itemName gameState = case findObject itemName (roomObjects $ currentRoom gameState) of
  Just obj -> case Map.lookup "door" (objectValues obj) of
    Just True -> do
      let (newState, resultMsg) = openDoor itemName gameState
      case resultMsg of
        Just msg -> do
          case msg of
            "You died!" -> do
              putStrLn msg
              exitSuccess
              return ()
            _ -> do
              putStrLn msg
              case newState of
                Just state -> gameLoop state
                Nothing -> gameLoop gameState
        Nothing -> return ()
    Just False -> do
      let (newState, resultMsg) = openObject itemName gameState
      case resultMsg of
        Just msg -> do
          putStrLn msg
          case newState of
            Just state -> gameLoop state
            Nothing -> gameLoop gameState
    Nothing -> invalidInput gameState
  Nothing -> invalidInput gameState

-- handleOpen itemName gameState = case
--   let (newState, resultMsg) = openDoor itemName gameState
--   case resultMsg of
--     Just msg -> do
--       putStrLn msg
--       case newState of
--         Just state -> gameLoop state
--         Nothing -> gameLoop gameState
--     Nothing -> return ()

handleUse :: String -> String -> GameState -> IO ()
handleUse itemName targetName gameState = do
  putStrLn ""
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
    printEmptyLine
    putStrLn $ "There is no " ++ itemName ++ " here."
    printEmptyLine
    gameLoop gameState

handleLook :: GameState -> IO ()
handleLook gameState = do
  printEmptyLine
  putStrLn $ roomDescription (currentRoom gameState)
  putStrLn ""
  putStrLn "You look around and see:"
  mapM_ (\obj -> putStrLn $ "  - " ++ objectName obj) (roomObjects $ currentRoom gameState)
  printEmptyLine
  gameLoop gameState

handleConnect :: String -> String -> String -> GameState -> IO ()
handleConnect cab1 cab2 cab3 gameState = do
  let (newState, resultMsg) = connectCables cab1 cab2 cab3 gameState
  case resultMsg of
    Just msg -> do
      putStrLn msg
      case newState of
        Just state -> gameLoop state
        Nothing -> gameLoop gameState
    Nothing -> return ()

handleCraftSuit :: GameState -> IO ()
handleCraftSuit gameState = do
  let (newState, resultMsg) = craftSuit gameState
  case resultMsg of
    Just msg -> do
      putStrLn msg
      case newState of
        Just state -> gameLoop state
        Nothing -> gameLoop gameState
    Nothing -> return ()

craftSuit :: GameState -> (Maybe GameState, Maybe String)
craftSuit gameState =
  if "Space_Suit_Trousers" `elem` map objectName (inventory gameState) && "Space_Suit_Jacket" `elem` map objectName (inventory gameState) && "Space_Suit_Helmet" `elem` map objectName (inventory gameState)
    then
      let newInvWithoutParts = filter (\o -> objectName o /= "Space_Suit_Trousers" && objectName o /= "Space_Suit_Jacket" && objectName o /= "Space_Suit_Helmet") (inventory gameState) ++ [spaceSuit]
       in ( Just $
              gameState
                { inventory = newInvWithoutParts,
                  currentRoom = currentRoom gameState,
                  allRooms = allRooms gameState
                },
            Just "\nYou crafted a space suit.\n"
          )
    else (Nothing, Just "\nYou don't have all those items.\n")

invalidInput :: GameState -> IO ()
invalidInput gameState = do
  printEmptyLine
  putStrLn "Invalid command. Try again."
  printEmptyLine
  gameLoop gameState

gameLoop :: GameState -> IO ()
gameLoop gameState = do
  -- printSeparator
  -- putStrLn $ roomDescription (currentRoom gameState)
  printSeparator
  putStr "What do you want to do? \n> "

  command <- getLine

  case words command of
    ["quit"] -> return ()
    ["craft", "Space_Suit_Trousers", "Space_Suit_Jacket", "Space_Suit_Helmet"] -> handleCraftSuit gameState
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
    ["list", "rooms"] -> do
      putStrLn $ listRooms gameState
      gameLoop gameState
    ["connect", cab1, cab2, cab3] -> handleConnect cab1 cab2 cab3 gameState
    _ -> invalidInput gameState
