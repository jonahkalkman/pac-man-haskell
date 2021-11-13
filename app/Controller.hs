module Controller where
import Model 
import System.Random

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

nextPosition :: Position -> Direction -> Position
nextPosition (x,y) dir | dir == North = (x, y - 1)
                       | dir == South = (x, y + 1)
                       | dir == West = (x - 1, y)
                       | dir == East = (x + 1, y)
                       | otherwise = (x,y)

isWall :: BoardItem -> Bool
isWall (Wall _) = True
isWall boardItem = False

collision :: Board -> Position -> Direction -> Bool
collision board pos dir = isWall boardItem
                          where
                            boardItem
                               | dir == East = row !! round x -- check if x is a whole number
                               | dir == West = row !! round x
                               | otherwise = row !! round x
                            row
                               | dir == South = board !! round y
                               | dir == North = board !! round y
                               | otherwise = board !! round y
                            (x,y) = nextPosition pos dir    

-- randomDirection :: IO Direction
-- randomDirection = do gen <- newStdGen
--                      let ns = randoms gen :: Direction
--                      return ns        

possibleGhostDirection :: Board -> Ghost -> [Direction]
possibleGhostDirection board ghost = possibleDirections
                                      where
                                        directions = [North, South, East, West]
                                        possibleDirections  = filter collision board (ghostPosition ghost) directions

-- Update world every frame
step :: Float -> GameState -> GameState
step sec gs = gs { player = newPlayer, ghosts = newGhosts }
            where
              currentDirection = playerDirection (player gs)
              currentPosition = playerPosition (player gs)
              currentBoard = board gs
              newPlayer = (player gs) {playerPosition = move currentBoard currentPosition currentDirection}
              newGhosts = map (\ghost -> ghost { ghostPosition = move currentBoard (ghostPosition ghost) (head possibleGhostDirection) }) (ghosts gs)

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