module Controller where
import Model 
import System.Random
import Control.Lens

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

possibleGhostPositions :: Board -> Ghost -> [(Position, Direction)]
possibleGhostPositions board ghost = filter (\pos -> not (collision board (fst pos))) possiblePositions
                                      where
                                        -- Excluding the backPosition from the nextPositions 
                                        possiblePositions | ghostDir == North = (nextPosition ghostPos North, North) : (nextPosition ghostPos West, West) : (nextPosition ghostPos East, East) : []
                                                          | ghostDir == South = (nextPosition ghostPos West, West) : (nextPosition ghostPos South, South) : (nextPosition ghostPos East, East) : []
                                                          | ghostDir == West = (nextPosition ghostPos North, North) : (nextPosition ghostPos West, West) : (nextPosition ghostPos South, South) : []
                                                          | ghostDir == East = (nextPosition ghostPos North, North) : (nextPosition ghostPos South, South) : (nextPosition ghostPos East, East) : []
                                                            where
                                                              ghostPos = ghostPosition ghost
                                                              ghostDir = ghostDirection ghost

ghostsTargetPosition :: GhostType -> Position -> Position 
ghostsTargetPosition Blinky playerPosition = playerPosition 
ghostsTargetPosition _ playerPosition = playerPosition  

ghostBestDirection :: [(Position, Direction)] -> Position -> Direction
ghostBestDirection possiblePositions targetPosition = head (tuplegetlowest (helperGhostBestDirection possiblePositions targetPosition))

helperGhostBestDirection :: [(Position, Direction)] -> Position -> [(Float, Direction)]
helperGhostBestDirection [] _ = []
helperGhostBestDirection ((a1,a2):xs) targetPosition = ((distancePoints a1 targetPosition), a2) : helperGhostBestDirection xs targetPosition

tuplegetlowest :: [(Float, Direction)] -> [Direction]
tuplegetlowest [] = []
tuplegetlowest list@((a1,a2):xs)
    | a1 == lowestDistance = a2 : tuplegetlowest xs
    | otherwise = tuplegetlowest xs
      where
        lowestDistance = minimum (map (\x -> fst x) list)

distancePoints :: Position -> Position -> Float
distancePoints positionOne positionTwo = (x2 - x1) ^ 2 + (y2 - y1) ^ 2
                                    where
                                      (x1,y1) = positionOne
                                      (x2,y2) = positionTwo

move :: Board -> Position -> Direction -> Position
move board pos dir = if hasCollision then pos else newPosition
          where
            hasCollision = collision board (nextPosition pos dir)
            newPosition | dir == North = (fst pos, snd pos - 1)
                        | dir == South = (fst pos, snd pos + 1)
                        | dir == West = (fst pos - 1, snd pos)
                        | dir == East = (fst pos + 1, snd pos)
                        | otherwise = pos

moveGhost :: Board -> Ghost -> Position ->  Ghost
moveGhost board ghost playerPosition = if hasCollision then ghost {ghostDirection = newDirection} else ghost {ghostPosition = newPosition, ghostDirection = newDirection}
          where
            pos = ghostPosition ghost
            dir = ghostDirection ghost
            ghostWithNewDirection = ghost {ghostDirection = newDirection}
            newDirection = ghostBestDirection (possibleGhostPositions board ghost) (ghostsTargetPosition (ghostType ghost) playerPosition)
            hasCollision = collision board (nextPosition pos newDirection)
            newPosition | newDirection == North = (fst pos, snd pos - 1)
                        | newDirection == South = (fst pos, snd pos + 1)
                        | newDirection == West = (fst pos - 1, snd pos)
                        | newDirection == East = (fst pos + 1, snd pos)
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

collision :: Board -> Position -> Bool
collision board pos = isWall (currentBoardItem board pos)

currentBoardItem :: Board -> Position -> BoardItem
currentBoardItem board pos = boardItem
                              where
                                boardItem = row !! round x 
                                row = board !! round y
                                (x,y) = pos    

consumePellet :: GameState -> Board -> Player -> GameState
consumePellet gs board p = if isPellet currentItem then gs { board = board & element (round (snd (playerPosition p))) . element (round (fst (playerPosition p))) .~ Floor, score = currentScore + 1 } else gs
    where 
      currentScore = score gs
      currentItem = currentBoardItem board (playerPosition p)

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
              newGhosts = map (\ghost -> moveGhost currentBoard ghost currentPosition) (ghosts gs)

-- Handle user input
input :: Event -> GameState -> GameState
input (EventKey (SpecialKey KeyUp) Down _ _) gs = if nextPosHasCollision then gs else gs { player = newPlayer }
                                where
                                  nextPosHasCollision = collision (board gs) (nextPosition (playerPosition p) North)
                                  newPlayer = p {playerDirection = North}
                                  p = player gs
input (EventKey (SpecialKey KeyDown) Down _ _) gs = if nextPosHasCollision then gs else gs { player = newPlayer }
                                where 
                                  nextPosHasCollision = collision (board gs) (nextPosition (playerPosition p) South)
                                  newPlayer = p {playerDirection = South}
                                  p = player gs
input (EventKey (SpecialKey KeyLeft) Down _ _) gs = if nextPosHasCollision then gs else gs { player = newPlayer }
                                where 
                                  nextPosHasCollision = collision (board gs) (nextPosition (playerPosition p) West) 
                                  newPlayer = p {playerDirection = West}
                                  p = player gs
input (EventKey (SpecialKey KeyRight) Down _ _) gs = if nextPosHasCollision then gs else gs { player = newPlayer }
                                where 
                                  nextPosHasCollision = collision (board gs) (nextPosition (playerPosition p) East)
                                  newPlayer = p {playerDirection = East}
                                  p = player gs
input _ gs = gs 