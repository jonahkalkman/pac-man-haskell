module Controller where
import Model 
import System.Random
import Control.Lens

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

move :: Board -> Position -> Direction -> Position
move board pos dir = if hasCollision then pos else newPosition
          where
            hasCollision = collision board pos dir
            newPosition | dir == North = (fst pos, snd pos - 1)
                        | dir == South = (fst pos, snd pos + 1)
                        | dir == West = (fst pos - 1, snd pos)
                        | dir == East = (fst pos + 1, snd pos)
                        | otherwise = pos

moveGhost :: Board -> Ghost -> Ghost
moveGhost board ghost = if hasCollision then moveGhost board ghostWithNewDirection else ghost {ghostPosition = newPosition}
          where
            pos = ghostPosition ghost
            dir = ghostDirection ghost
            ghostWithNewDirection = ghost {ghostDirection = newDirection}
            newDirection = head (possibleGhostDirection board pos)
            hasCollision = collision board pos dir
            newPosition | dir == North = (fst pos, snd pos - 1)
                        | dir == South = (fst pos, snd pos + 1)
                        | dir == West = (fst pos - 1, snd pos)
                        | dir == East = (fst pos + 1, snd pos)
                        | otherwise = pos

nextPosition :: Position -> Direction -> Position
nextPosition (x,y) dir | dir == North = (x, y - 1)
                       | dir == South = (x, y + 1)
                       | dir == West = (x - 1, y)
                       | dir == East = (x + 1, y)
                       | otherwise = (x,y)

isWall :: BoardItem -> Bool
isWall (Wall _) = True
isWall boardItem = False

isPellet :: BoardItem -> Bool
isPellet (Pellet _) = True
isPellet boardItem = False

collision :: Board -> Position -> Direction -> Bool
collision board pos dir = isWall (currentBoardItem board (nextPosition pos dir) dir)

currentBoardItem :: Board -> Position -> Direction -> BoardItem
currentBoardItem board pos dir = boardItem
                                  where
                                    boardItem
                                        | dir == East = row !! round x -- check if x is a whole number
                                        | dir == West = row !! round x
                                        | otherwise = row !! round x
                                    row
                                        | dir == South = board !! round y
                                        | dir == North = board !! round y
                                        | otherwise = board !! round y
                                    (x,y) = pos    

consumePellet :: GameState -> Board -> Player -> GameState
consumePellet gs board p = if isPellet currentItem then gs { board = board & element (round (snd (playerPosition p))) . element (round (fst (playerPosition p))) .~ Floor, score = currentScore + 1 } else gs
    where 
      currentScore = score gs
      currentItem = currentBoardItem board (playerPosition p) (playerDirection p)


possibleGhostDirection :: Board -> Position -> [Direction]
possibleGhostDirection board position = possibleDirections
                                          where
                                            directions = [North, South, East, West]
                                            possibleDirections = filter (\dir -> not(collision board position dir)) directions

-- Update world every frame
step :: Float -> GameState -> GameState
step sec gs = gs { player = newPlayer, ghosts = newGhosts, board = newBoard, score = newScore }
            where
              currentDirection = playerDirection (player gs)
              currentPosition = playerPosition (player gs)
              currentBoard = board gs
              newBoard = board boardAfterConsume
              newScore = score boardAfterConsume
              boardAfterConsume = consumePellet gs currentBoard (player gs)
              newPlayer = (player gs) {playerPosition = move currentBoard currentPosition currentDirection }
              newGhosts = map (\ghost -> moveGhost currentBoard ghost) (ghosts gs)

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