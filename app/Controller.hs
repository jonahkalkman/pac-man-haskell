module Controller where

import Model 
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


-- Update position for direction
move :: Position -> Direction -> Float -> Position
move pos direction sec = newPosition
          where
            newPosition | direction == North = (fst pos, snd pos + 20 * sec)
                        | direction == South = (fst pos, snd pos - 20 * sec)
                        | direction == West = (fst pos - 20 * sec, snd pos)
                        | direction == East = (fst pos + 20 * sec, snd pos)
                        | otherwise = pos

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