import Data.List

-- Define the Room data type
data Room = Room {roomId :: Int, roomName :: String, roomDescription :: String, roomItems :: [Item]}

-- Define the Item data type
data Item = Item {itemId :: Int, itemName :: String, itemDescription :: String}

-- Define the Player data type
data Player = Player {playerName :: String, playerInventory :: [Item]}

-- Function to print a separator line
printSeparator :: IO ()
printSeparator = putStrLn "----------------------------------------"

-- Function to print the room description and available items
printRoom :: Room -> IO ()
printRoom room = do
  putStrLn $ "You are in " ++ roomName room ++ ". " ++ roomDescription room
  putStrLn "Items in the room:"
  mapM_ (\item -> putStrLn $ "- " ++ itemName item ++ ": " ++ itemDescription item) (roomItems room)

-- Function to print the player's inventory
printInventory :: Player -> IO ()
printInventory player = do
  putStrLn "Your inventory:"
  mapM_ (\item -> putStrLn $ "- " ++ itemName item) (playerInventory player)

-- Function to pick up an item and add it to the player's inventory
pickUpItem :: Player -> Item -> Player
pickUpItem player item = player {playerInventory = item : playerInventory player}

-- Function to use an item on a specific object in the room
useItem :: Player -> Room -> Item -> IO ()
useItem player room item = do
  putStrLn $ "You use the " ++ itemName item ++ " in the " ++ roomName room ++ "."
  -- You can add more complex logic for different item-object interactions here
  printSeparator

-- Main game loop
gameLoop :: Player -> Room -> IO ()
gameLoop player currentRoom = do
  printRoom currentRoom
  printInventory player
  putStrLn "What do you want to do?"
  putStrLn "1. Move to another room"
  putStrLn "2. Pick up an item"
  putStrLn "3. Use an item"
  putStrLn "4. Quit"

  choice <- getLine
  case choice of
    "1" -> do
      putStrLn "Enter the room number to move to:"
      newRoomId <- getLine
      let newRoom = if newRoomId == "1" then room1 else room2
      gameLoop player newRoom
    "2" -> do
      putStrLn "Enter the item number to pick up:"
      itemNumber <- getLine
      let item = findItemById (read itemNumber) (roomItems currentRoom)
      case item of
        Just i -> gameLoop (pickUpItem player i) currentRoom
        Nothing -> do
          putStrLn "Invalid item number."
          gameLoop player currentRoom
    "3" -> do
      putStrLn "Enter the item number to use:"
      itemNumber <- getLine
      let item = findItemById (read itemNumber) (playerInventory player)
      case item of
        Just i -> useItem player currentRoom i >> gameLoop player currentRoom
        Nothing -> do
          putStrLn "Invalid item number."
          gameLoop player currentRoom
    "4" -> putStrLn "Goodbye!"
    _ -> do
      putStrLn "Invalid choice. Please enter a valid option."
      gameLoop player currentRoom

-- Function to find an item by its ID in a list of items
findItemById :: Int -> [Item] -> Maybe Item
findItemById targetId items = find (\item -> itemId item == targetId) items

-- Define two rooms and some items
room1 :: Room
room1 =
  Room
    { roomId = 1,
      roomName = "Room 1",
      roomDescription = "a small room with a table",
      roomItems = [Item 1 "Key" "a shiny key", Item 2 "Book" "an old book"]
    }

room2 :: Room
room2 =
  Room
    { roomId = 2,
      roomName = "Room 2",
      roomDescription = "a large room with a locked door",
      roomItems = [Item 3 "Candle" "a burning candle", Item 4 "Map" "a detailed map"]
    }

-- Entry point of the program
main :: IO ()
main = do
  putStrLn "Welcome to the Text-Based Game in Haskell!"
  putStrLn "Enter your name:"
  playerName <- getLine
  let player = Player {playerName = playerName, playerInventory = []}
  putStrLn "You find yourself in a mysterious place. Let the adventure begin!"
  gameLoop player room1
