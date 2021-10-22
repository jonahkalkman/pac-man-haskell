module Main where

import Controller
import Model
import View

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
      wallImg <- loadBMP "assets/wall.bmp"

      play 
        (InWindow "Pac-Man" (448, 576) (0, 0))
        red                   -- Background color
        10                    -- Frames per second
        initialState          -- Initial state
        (`render` [wallImg])  -- View function
        input                 -- Event function
        step                  -- Step function