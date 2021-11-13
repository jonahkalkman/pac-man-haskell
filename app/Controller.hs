module Controller where


import Model 
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


-- Updates users position with 1 grid position every frame (64 frames per second), so 64 moves per second
move :: Board -> Position -> Direction -> Float -> Position
move board pos dir seconds = if hasCollision then pos else newPosition
          where
            hasCollision = collision board pos dir
            newPosition | dir == North = (fst pos, snd pos - 10*seconds)
                        | dir == South = (fst pos, snd pos + 10*seconds)
                        | dir == West = (fst pos - 10*seconds, snd pos)
                        | dir == East = (fst pos + 10*seconds, snd pos)
                        | otherwise = pos

roundpos :: Position -> Position
roundpos (x,y) = (realToFrac(round x), realToFrac(round y))

-- realToFrac :: (Real a, Fractional b) => a -> b
-- realToFrac = fromRational . toRational


-- xymove :: Direction -> Direction -> Bool
-- -- xymove prevdir dir | prevdir == North && dir == East || dir == West || prevdir == South && dir == East || dir == West = True
-- --                    | prevdir == East && dir == North || dir == South || prevdir == West && dir == North || dir == South = True
-- --                    | otherwise = False

-- should return true if iswhole returns false
-- xymove :: Direction -> Direction -> Position -> Bool
-- xymove prevdir dir (x,y) | ns || ew = True
--                        | otherwise = False
--                        where
--                          ns = if iswhole2 x then True else False
--                          ew = if iswhole2 y then True else False
--                          ns1 = prevdir == North || prevdir == South && dir == North || dir == South
--                          ns2 = prevdir == East || prevdir == West && dir == East || dir == West
--                          iswhole' = iswhole2 x' && iswhole2 y'
--                          (x', y') = nextPosition (x,y) dir

-- helperNextPos :: Position -> Direction -> Bool
-- helperNextPos (x,y) dir | dir == North && iswhole2 x = True
--                     | dir == South && iswhole2 x = True
--                     | dir == West && iswhole2 y = True
--                     | dir == East && iswhole2 y = True
--                     | otherwise = False   

iswhole2 :: Float -> Bool
iswhole2 x = isInt x 1                   
                       
-- predicate to check if current position equals a rounded number
iswhole :: (Float, Float) -> Bool
iswhole (x,y) | isInt x 1 && isInt y 1 = True
              | otherwise = False

isInt :: (Integral a, RealFrac b) => b -> a -> Bool
isInt x n = (round $ 10^(fromIntegral n)*(x-(fromIntegral $ round x)))==0


-- Calculates the next position of the player using the direction of the player
nextPosition :: Position -> Direction -> Position
nextPosition (x,y) dir | dir == North = (x, y - 0.51)
                       | dir == South = (x, y + 0.51)
                       | dir == West = (x - 0.51, y)
                       | dir == East = (x + 0.51, y)
                       | otherwise = (x,y)


                   

turnintofloat :: Float -> Int
turnintofloat x = round x


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
collision board pos dir = if isWall boardItem then True else False
                          where
                            boardItem
                               | dir == East = row !! round x -- check if x is a whole number
                               | dir == West = row !! round x
                              --  | dir == North = row !! ceiling x
                              --  | dir == South = row !! floor x
                               | otherwise = row !! round x
                            row
                               | dir == South = board !! round y
                               | dir == North = board !! round y
                              --  | dir == East = board !! ceiling y
                              --  | dir == West = board !! floor y
                               | otherwise = board !! round y
                            -- (x,y) = nextPosition pos dir   
                            (x,y) = nextPosition pos dir                    

-- if direction = east or west && next direction = north or south -> round 


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