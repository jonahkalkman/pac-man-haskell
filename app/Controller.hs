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

ghostRandomDirection :: [(Position, Direction)] -> Direction 
ghostRandomDirection possiblePositions = snd (last possiblePositions)

helperGhostBestDirection :: [(Position, Direction)] -> Position -> [(Float, Direction)]
helperGhostBestDirection [] _ = []
helperGhostBestDirection ((a1,a2):xs) targetPosition = (distancePoints a1 targetPosition, a2) : helperGhostBestDirection xs targetPosition

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
            hasCollision = collision board (nextPosition pos newDirection)
            newDirection | ghostStatus ghost == Chase = ghostBestDirection (possibleGhostPositions board ghost) (ghostsTargetPosition (ghostType ghost) playerPosition)
                         | ghostStatus ghost == Frightened = ghostRandomDirection (possibleGhostPositions board ghost)
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
consumePellet gs board p | currentItem == Pellet NormalPellet = gs { board = board & element (round (snd (playerPosition p))) . element (round (fst (playerPosition p))) .~ Floor, score = currentScore + 1 }
                         | currentItem == Pellet PowerPellet = gs { board = board & element (round (snd (playerPosition p))) . element (round (fst (playerPosition p))) .~ Floor, ghosts = newGhosts }
                         | otherwise = gs 
                            where 
                              newGhosts = map updateGhostStatus (ghosts gs)
                              updateGhostStatus g = g {ghostStatus = Frightened}
                              currentScore = score gs
                              currentItem = currentBoardItem board (playerPosition p)

collideChase :: GameState -> Board -> Ghost -> Player -> Bool
collideChase gs board ghost player = ghostPos == playerPos && ghostStatus ghost == Chase
                                        where
                                          playerPos = playerPosition player
                                          ghostPos = ghostPosition ghost

collideFrightened :: GameState -> Board -> Ghost -> Player -> Bool
collideFrightened gs board ghost player = ghostPos == playerPos && ghostStatus ghost == Frightened
                                        where
                                          playerPos = playerPosition player
                                          ghostPos = ghostPosition ghost                                          

collideGhost :: [Bool] -> Bool
collideGhost [] = False
collideGhost (x:xs) | x == True = True
                    | otherwise = collideGhost xs

randomGo :: Int -> Int -> IO Int
randomGo x y = getStdRandom (randomR (x,y))

randomPos :: (Int,Int) -> Board -> Board
randomPos (x,y) board | x == 1 = board & element (y) . element 6 .~ Pellet PowerPellet
                      | x == 2 = board & element (y) . element 21 .~ Pellet PowerPellet
writeHighScore :: GameState -> IO GameState
writeHighScore gs = do 
                      a <- randomGo 1 2
                      b <- randomGo 3 26
                      currentHighScore <- readFile "highscores.txt"
                      if score gs > read currentHighScore then writeFile "highscores.txt" (show (score gs)) else writeFile "highscores.txt" currentHighScore
                      return initialState {board = randomPos (a,b) initialBoard}
                                        
-- Update world every frame
step :: Float -> GameState -> IO GameState
step sec gs | paused gs = return gs
            | collidingChase = writeHighScore gs
            | collidingFrightened = return (gs { player = newPlayer, ghosts = ghostsAfterEaten, score = newScore + 100})
            | otherwise = return (gs { player = newPlayer, ghosts = ghostsAfterConsume, board = newBoard, score = newScore, ghostFrightenedAnimation = not(ghostFrightenedAnimation gs), powerPelletAnimation = not(powerPelletAnimation gs) })
            where
              currentDirection = playerDirection (player gs)
              currentPosition = playerPosition (player gs)
              currentBoard = board gs
              newBoard = board boardAfterConsume
              newScore = score boardAfterConsume
              ghostsAfterConsume = ghosts boardAfterConsume
              ghostsAfterEaten = map (\ghost -> ghost {ghostPosition = (1,3), ghostStatus = Chase}) (ghosts gs)
              boardAfterConsume = consumePellet (gs {ghosts = newGhosts}) currentBoard (player gs)
              collidingChase = collideGhost (map (\ghost -> collideChase gs currentBoard ghost (player gs)) (ghosts gs))
              collidingFrightened = collideGhost (map (\ghost -> collideFrightened gs currentBoard ghost (player gs)) (ghosts gs))
              newPlayer = (player gs) {playerPosition = move currentBoard currentPosition currentDirection }
              newGhosts = map (\ghost -> moveGhost currentBoard ghost currentPosition) (ghosts gs)

-- Handle user input
input :: Event -> GameState -> IO GameState
-- Added pause button
input (EventKey (Char 'p') Down _ _) gs = if (paused gs) == False then return (gs { paused = True }) else return (gs { paused = False })                                
input (EventKey (SpecialKey KeyUp) Down _ _) gs = if nextPosHasCollision then return gs else return gs { player = newPlayer }
                                where
                                  nextPosHasCollision = collision (board gs) (nextPosition (playerPosition p) North)
                                  newPlayer = p {playerDirection = North}
                                  p = player gs
input (EventKey (SpecialKey KeyDown) Down _ _) gs = if nextPosHasCollision then return gs else return gs { player = newPlayer }
                                where 
                                  nextPosHasCollision = collision (board gs) (nextPosition (playerPosition p) South)
                                  newPlayer = p {playerDirection = South}
                                  p = player gs
input (EventKey (SpecialKey KeyLeft) Down _ _) gs = if nextPosHasCollision then return gs else return gs { player = newPlayer }
                                where 
                                  nextPosHasCollision = collision (board gs) (nextPosition (playerPosition p) West) 
                                  newPlayer = p {playerDirection = West}
                                  p = player gs
input (EventKey (SpecialKey KeyRight) Down _ _) gs = if nextPosHasCollision then return gs else return gs { player = newPlayer }
                                where 
                                  nextPosHasCollision = collision (board gs) (nextPosition (playerPosition p) East)
                                  newPlayer = p {playerDirection = East}
                                  p = player gs
input _ gs = return gs 