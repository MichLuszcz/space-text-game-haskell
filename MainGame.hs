{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

import Control.Monad (guard)
import Control.Monad.RWS.Class (MonadState (put))
import Data.List
import Data.Map.Strict qualified as Map
--
-- Module imports
--
import DataTypes
import DoorHandling
import GameObjects
import InstructionHandlers
import ObjectManagment
import PlayerActions
import Text.XHtml (target)
import UseHandling
import UtilityFunctions

-- Main function to start the game
main :: IO ()
main = do
  printSeparator
  putStrLn "Welcome to Haskell Adventure."
  printSeparator
  printInstructions

  putStrLn "A loud crashing sound wakes you up in you bed inside the engineering crew bedroom. You look around and see that the room is in a mess. A vent in the east corner of the room swings wide open. Blaring alarms can be heard from the main corridor on the south side. You see a locker, a desk and a bed. You need to act, fast."
  printEmptyLine
  putStrLn "You are at the crew bedroom."
  gameLoop initialState
