module Controller where

import Model 
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> IO GameState
step _ gstate = return gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input _ gstate = return gstate