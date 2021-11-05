module Controller where

import Model 
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- Updates users position with 1 grid position every frame (64 frames per second), so 64 moves per second
move :: Position -> Direction -> Float -> Position
move pos direction seconds = newPosition
          where
            newPosition | direction == North = (fst pos, snd pos - 1)
                        | direction == South = (fst pos, snd pos + 1)
                        | direction == West = (fst pos - 1, snd pos)
                        | direction == East = (fst pos + 1, snd pos)
                        | otherwise = pos

-- Calculates the next position of the player using the direction of the player
nextPosition :: Position -> Direction -> Position
nextPosition (x,y) dir | dir == North = (x, y - 1)
                       | dir == South = (x, y + 1)
                       | dir == West = (x - 1, y)
                       | dir == East = (x + 1, y)
                       | otherwise = (x,y)

-- Checks if a BoardItem is a Wall (QuickFix because of WallType)
isWall :: BoardItem -> Bool
isWall (Wall _) = True
isWall boardItem = False

-- TODO: Because using ceiling we have multiple places where the player can go into a path, 1.4 ceiling is 2.0. 
-- Collision based on direction does not change speed, but goes further
collision :: Board -> Position -> Direction -> Bool
collision board pos dir = isWall boardItem
                          where
                            boardItem = row !! ceiling x
                            row = board !! ceiling y
                            (x,y) = nextPosition pos dir                         

-- Update world every frame
step :: Float -> GameState -> GameState
step sec gs = if hasCollision then gs else gs { player = newPlayer }
            where
              currentDirection = playerDirection (player gs)
              currentPosition = playerPosition (player gs)
              currentBoard = board gs
              hasCollision = collision currentBoard currentPosition currentDirection
              newPlayer = (player gs) {playerPosition = move currentPosition currentDirection sec}

-- Handle user input
input :: Event -> GameState -> GameState
input (EventKey (SpecialKey KeyUp) Down _ _) gs = if nextPosHasCollision then gs else gs { player = newPlayer }
                                where
                                  nextPosHasCollision = collision (board gs) (playerPosition p) North
                                  newPlayer = p {playerDirection = North}
                                  p = player gs
input (EventKey (SpecialKey KeyDown) Down _ _) gs = if nextPosHasCollision then gs else gs { player = newPlayer }
                                where 
                                  nextPosHasCollision = collision (board gs) (playerPosition p) South
                                  newPlayer = p {playerDirection = South}
                                  p = player gs
input (EventKey (SpecialKey KeyLeft) Down _ _) gs = if nextPosHasCollision then gs else gs { player = newPlayer }
                                where 
                                  nextPosHasCollision = collision (board gs) (playerPosition p) West
                                  newPlayer = p {playerDirection = West}
                                  p = player gs
input (EventKey (SpecialKey KeyRight) Down _ _) gs = if nextPosHasCollision then gs else gs { player = newPlayer }
                                where 
                                  nextPosHasCollision = collision (board gs) (playerPosition p) East
                                  newPlayer = p {playerDirection = East}
                                  p = player gs
input _ gs = gs 