module UseHandling where

import Control.Monad.RWS.Lazy (MonadState (put))
import Data.List
import qualified Data.Map.Strict as Map
import DataTypes
import Distribution.Compat.Lens (use)
import GameObjects
import ObjectManagment
import Text.XHtml (target)

-- Function to handle the use command like openDoor from DoorHandling.hs
useItem :: String -> String -> GameState -> (Maybe GameState, Maybe String)
useItem itemName targetName gameState =
  case findObject itemName (inventory gameState) of
    Just obj -> case Map.lookup "usable" (objectValues obj) of
      Just True ->
        -- Make a case of from the itemName and targetName, and the cases should call separate functions like keyUnlockNorthDoor
        case (itemName, targetName) of
          ("Desk_Key", "Desk") -> keyUnlock itemName targetName gameState
          ("Crew_Access_Card", "Security_Door") -> keyUnlock itemName targetName gameState
          ("Hammer", "Supply_Cabinet") -> hammerCabinet gameState
          ("Thick_Blanket", "Electric_Box") -> putOutFire gameState
          ("Universal_Speech_Translator", "Wounded_Engineering_Chief") -> translateChief gameState
          ("Cyber_Key", "South_Corridor_Exit_Door") -> keyUnlock itemName targetName gameState
          _ -> (Nothing, Just ("You can't figure out how to use " ++ itemName ++ " on " ++ targetName ++ ".\n"))
      Just False -> (Nothing, Just "This item cannot be used.")
      _ -> (Nothing, Just "This item cannot be used.")
    Nothing -> (Nothing, Just "Item not found")

-- Create a keyUnlock function that takes a doorName and a GameState sets the value of "openable" to True for the doorName
keyUnlock :: String -> String -> GameState -> (Maybe GameState, Maybe String)
keyUnlock keyName doorName gameState =
  let updatedRoom = (currentRoom gameState) {roomObjects = map (\obj -> if objectName obj == doorName then obj {objectValues = Map.insert "openable" True (objectValues obj)} else obj) (roomObjects $ currentRoom gameState)}
      -- Remove the key from the inventory
      updatedInventory = filter (\obj -> objectName obj /= keyName) (inventory gameState)
      -- Create newAllRooms, where the InitialRoom is replaced with the updatedRoom
      currentRoomObj = currentRoom gameState
   in (Just $ gameState {currentRoom = updatedRoom, allRooms = Map.insert (roomName currentRoomObj) updatedRoom (allRooms gameState), inventory = updatedInventory}, Just ("\n" ++ doorName ++ " succesfully unlocked.\n"))

-- Create a hammerCabinet function that takes a GameState and sets the value of "openable" to True for the Supply_Cabinet, removes the hammer and prints a message
hammerCabinet :: GameState -> (Maybe GameState, Maybe String)
hammerCabinet gameState =
  let updatedRoom = (currentRoom gameState) {roomObjects = map (\obj -> if objectName obj == "Supply_Cabinet" then obj {objectValues = Map.insert "openable" True (objectValues obj)} else obj) (roomObjects $ currentRoom gameState)}
      -- Remove the hammer from the inventory
      updatedInventory = filter (\obj -> objectName obj /= "Hammer") (inventory gameState)
      -- Create newAllRooms, where the InitialRoom is replaced with the updatedRoom
      currentRoomObj = currentRoom gameState
   in (Just $ gameState {currentRoom = updatedRoom, allRooms = Map.insert (roomName currentRoomObj) updatedRoom (allRooms gameState), inventory = updatedInventory}, Just "\nAh yes, brute force. Always a good solution. \nAfter smashing the cabinet open with a hammer, you can look for anything useful that you can find inside. \n")

putOutFire :: GameState -> (Maybe GameState, Maybe String)
putOutFire gameState =
  let updatedRoom = (currentRoom gameState) {roomObjects = map (\obj -> if objectName obj == "Electric_Box" then obj {objectDescription = "The Electric_Box is not burning anymore"} else obj) (roomObjects $ currentRoom gameState)}
      -- Remove the hammer from the inventory
      newRoomObjects = southCorridorExitDoor : roomObjects updatedRoom
      newRoomObjects2 = woundedEngineeringChief : newRoomObjects
      newRoomObjects3 = cantineEntranceDoor : newRoomObjects2
      updatedInventory = filter (\obj -> objectName obj /= "Thick_Blanket") (inventory gameState)

      updatedRoom2 = updatedRoom {roomObjects = newRoomObjects3}
      -- Create newAllRooms, where the InitialRoom is replaced with the updatedRoom
      currentRoomObj = currentRoom gameState
   in (Just $ gameState {currentRoom = updatedRoom2, allRooms = Map.insert (roomName currentRoomObj) updatedRoom2 (allRooms gameState), inventory = updatedInventory}, Just "You put out the fire with the blanket. \nThe electric box is out. You can see the rest of the room clearly now. \n")

-- When using the transaltor, change the description of the wounded engineering chief to the translated one
translateChief :: GameState -> (Maybe GameState, Maybe String)
translateChief gameState =
  let updatedRoom =
        (currentRoom gameState)
          { roomObjects =
              map
                ( \obj ->
                    if objectName obj
                      == "Wounded_Engineering_Chief"
                      then
                        obj
                          { objectDescription =
                              "You: Hey, chief, are you okay? What happened? \n\
                              \Qaux'ods: I'm not sure. I was in the cantine, when the ship started shaking. I ran out to see what's going on and I saw a bright flash of light. \n\
                              \ The captain told me through the radio (plot_element), but as soon as he started explaining, the radio went silent. \n\
                              \ I ran to the main corridor to see if I can help anyone, but I was hit by a piece of debris. I'm not sure how long I can hold on. \n\
                              \ You: I'm sure I can help you somehow! We can get out of here togheter! \n\
                              \ Qaux'ods: No, I'm afraid it's too late for me. You need to go on and survive. The ship took a heavy blow, it won't hold on for long. \n\
                              \ Take my access card, it will open up my office, it will help you get to the escape pod. Get there and escape. \n\
                              \ You: Thank you, chief. I will never forget what you've done for me. \n\
                              \ Qaux'ods: It's nothing but my duty. Now go on, I need to rest. \n"
                          }
                      else obj
                )
                (roomObjects $ currentRoom gameState)
          }
      -- Remove the translator from the inventory
      updatedInventory = filter (\obj -> objectName obj /= "Universal_Speech_Translator") (inventory gameState)
      updatedInventory2 = engineeringChiefAccessCard : updatedInventory
      -- Create newAllRooms, where the InitialRoom is replaced with the updatedRoom
      currentRoomObj = currentRoom gameState
   in (Just $ gameState {currentRoom = updatedRoom, allRooms = Map.insert (roomName currentRoomObj) updatedRoom (allRooms gameState), inventory = updatedInventory2}, Just "You used the translator on the wounded engineering chief. \n")
