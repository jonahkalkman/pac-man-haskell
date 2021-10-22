module Controller where

import Model 
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

step :: Float -> GameState -> GameState
step _ gstate = gstate

-- | Handle user input
input :: Event -> GameState -> GameState
input _ gstate = gstate