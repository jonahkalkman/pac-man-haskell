module Main where

import Controller
import Model
import View

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
      -- Loading all sprites
      wall0 <- loadBMP "sprites/left.bmp"
      wall1 <- loadBMP "sprites/right.bmp"
      wall2 <- loadBMP "sprites/bottom.bmp"
      wall3 <- loadBMP "sprites/top.bmp"

      wall4 <- loadBMP "sprites/bottomleftcorner.bmp"
      wall5 <- loadBMP "sprites/bottomrightcorner.bmp"
      wall6 <- loadBMP "sprites/topleftcorner.bmp"
      wall7 <- loadBMP "sprites/toprightcorner.bmp"

      wall8 <- loadBMP "sprites/doubleleft.bmp"
      wall9 <- loadBMP "sprites/doubleright.bmp"
      wall10 <- loadBMP "sprites/doublebottom.bmp"
      wall11 <- loadBMP "sprites/doubletop.bmp"

      wall12 <- loadBMP "sprites/doubletopleft.bmp"
      wall13 <- loadBMP "sprites/doubletopright.bmp"
      wall14 <- loadBMP "sprites/doublebottomleft.bmp"
      wall15 <- loadBMP "sprites/doublebottomright.bmp"

      wall16 <- loadBMP "sprites/doubleinleft.bmp"
      wall17 <- loadBMP "sprites/doubleinright.bmp"
      wall18 <- loadBMP "sprites/doubleinupleft.bmp"
      wall19 <- loadBMP "sprites/doubleinupright.bmp"

      wall20 <- loadBMP "sprites/doublesquaretopleft.bmp"
      wall21 <- loadBMP "sprites/doublesquaretopright.bmp"
      wall22 <- loadBMP "sprites/doublesquarebottomleft.bmp"
      wall23 <- loadBMP "sprites/doublesquarebottomright.bmp"

      wall24 <- loadBMP "sprites/doublesquareleft.bmp"
      wall25 <- loadBMP "sprites/doublesquareright.bmp"

      wall26 <- loadBMP "sprites/bottomleftcornerinside.bmp"
      wall27 <- loadBMP "sprites/bottomrightcornerinside.bmp"
      wall28 <- loadBMP "sprites/topleftcornerinside.bmp"
      wall29 <- loadBMP "sprites/toprightcornerinside.bmp"

      blank <- loadBMP "sprites/blank.bmp"
      dotpiece <- loadBMP "sprites/dotpiece.bmp"
      -- TODO: Code cleaning, rename to normalpellet


      playerImg <- loadBMP "assets/pacmanright1.bmp"
      -- TODO: add more pacman sprites for every direction + animations

      -- TODO: add highscore txt file with openfile
      -- https://hoogle.haskell.org/?hoogle=openFile

      play 
        -- TO DO: resolution somehow incorrect. change background to red to see the differences.
        (InWindow "Pac-Man" (448, 576) (0, 0))
        black                   -- Background color
        64                   -- Frames per second
        initialState          -- Initial state
        (`render` [wall0, wall1, wall2, wall3, wall4, wall5, wall6, wall7, wall8, wall9, wall10, wall11, wall12, wall13, wall14, wall15, wall16, wall17, wall18, wall19, wall20, wall21, wall22, wall23, wall24, wall25, wall26, wall27, wall28, wall29, blank, dotpiece, playerImg])  -- View function
        input                 -- Event function
        step                  -- Step function