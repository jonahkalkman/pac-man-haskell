module Controller where

import Model 
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- Updates users position with 1 grid position every frame (64 frames per second), so 64 moves per second
move :: Board -> Position -> Direction -> Float -> Position
move board pos dir seconds = if hasCollision then pos else newPosition
          where
            hasCollision = collision board pos dir
            newPosition | dir == North = (fst pos, snd pos - 0.05)
                        | dir == South = (fst pos, snd pos + 0.05)
                        | dir == West = (fst pos - 0.05, snd pos)
                        | dir == East = (fst pos + 0.05, snd pos)
                        | otherwise = pos

-- Calculates the next position of the player using the direction of the player
nextPosition :: Position -> Direction -> Position
nextPosition (x,y) dir | dir == North = (x, y - 0.05)
                       | dir == South = (x, y + 0.05)
                       | dir == West = (x - 0.05, y)
                       | dir == East = (x + 0.05, y)
                       | otherwise = (x,y)

consume :: BoardItem -> Score
consume bi | isPellet bi = 100
           | otherwise = 0

-- Checks if a BoardItem is a Wall (QuickFix because of WallType)
isWall :: BoardItem -> Bool
isWall (Wall _) = True
isWall boardItem = False

isPellet :: BoardItem -> Bool
isPellet (Pellet _) = True
isPellet boardItem = False

-- Collision based on direction does not change speed, but goes further
collision :: Board -> Position -> Direction -> Bool
collision board pos dir = isWall boardItem
                          where
                            boardItem = row !! round x
                            row = board !! round y
                            (x,y) = nextPosition pos dir                         

-- Update world every frame
step :: Float -> GameState -> GameState
step sec gs = gs { player = newPlayer }
            where
              currentDirection = playerDirection (player gs)
              currentPosition = playerPosition (player gs)
              currentBoard = board gs
              newPlayer = (player gs) {playerPosition = move currentBoard currentPosition currentDirection sec}

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