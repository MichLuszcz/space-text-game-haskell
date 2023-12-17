{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

import Control.Monad (guard)
import Control.Monad.RWS.Class (MonadState (put))
import Data.List
import qualified Data.Map.Strict as Map
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
  gameLoop initialState
