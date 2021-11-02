module Controller where

import Model 
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- Update position for direction
move :: Position -> Direction -> Float -> Position
move pos direction sec = newPosition
          where
            newPosition | direction == North = (fst pos, snd pos + 1 * sec)
                        | direction == South = (fst pos, snd pos - 1 * sec)
                        | direction == West = (fst pos - 1 * sec, snd pos)
                        | direction == East = (fst pos + 1 * sec, snd pos)
                        | otherwise = pos

-- TODO: Collision; doen we collision voor walls of ook gelijk voor ghosts?
-- Implementen als helper function voor move?

-- wallCollision:: Position -> Direction -> Board -> (Not sure wat er returned moet worden, assuming gamestate)
-- expr =  pacman positie + een stap verder op basis van direction van pac man. bijv, kijkt naar rechts en staat op (0,0) dan wordt het (1,0), als pacman naar links ijkt, (-1,0), uitgaand van (x,y)
-- Check if expr is of type wall, is true, stop moving?

-- Update world every frame
step :: Float -> GameState -> GameState
step sec gs = gs { player = newPlayer }
            where
              currentDirection = playerDirection (player gs)
              currentPosition = playerPosition (player gs)
              newPlayer =  (player gs) {playerPosition = move currentPosition currentDirection sec}

-- Handle user input
input :: Event -> GameState -> GameState
input (EventKey (SpecialKey KeyUp) Down _ _) gs = gs { player = newPlayer }
                                where
                                  newPlayer = p {playerDirection = North}
                                  p = player gs
input (EventKey (SpecialKey KeyDown) Down _ _) gs = gs { player = newPlayer }
                                where 
                                  newPlayer = p {playerDirection = South}
                                  p = player gs
input (EventKey (SpecialKey KeyLeft) Down _ _) gs = gs { player = newPlayer }
                                where 
                                  newPlayer = p {playerDirection = West}
                                  p = player gs
input (EventKey (SpecialKey KeyRight) Down _ _) gs = gs { player = newPlayer }
                                where 
                                  newPlayer = p {playerDirection = East}
                                  p = player gs
input _ gs = gs 