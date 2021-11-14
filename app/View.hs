module View where

import Graphics.Gloss
import Model

-- Render all possible Walls and items on the initial board
renderBoardItem :: BoardItem -> GameState -> Float -> Float -> [Picture] -> Picture
renderBoardItem boardItem gs xIndex yIndex images
  | boardItem == Wall 0 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (head images)
  | boardItem == Wall 1 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 1)
  | boardItem == Wall 2 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 2)
  | boardItem == Wall 3 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 3)
  | boardItem == Wall 4 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 4)
  | boardItem == Wall 5 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 5)
  | boardItem == Wall 6 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 6)
  | boardItem == Wall 7 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 7)
  | boardItem == Wall 8 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 8)
  | boardItem == Wall 9 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 9)
  | boardItem == Wall 10 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 10)
  | boardItem == Wall 11 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 11)
  | boardItem == Wall 12 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 12)
  | boardItem == Wall 13 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 13)
  | boardItem == Wall 14 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 14)
  | boardItem == Wall 15 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 15)
  | boardItem == Wall 16 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 16)
  | boardItem == Wall 17 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 17)
  | boardItem == Wall 18 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 18)
  | boardItem == Wall 19 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 19)
  | boardItem == Wall 20 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 20)
  | boardItem == Wall 21 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 21)
  | boardItem == Wall 22 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 22)
  | boardItem == Wall 23 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 23)
  | boardItem == Wall 24 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 24)
  | boardItem == Wall 25 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 25)
  | boardItem == Wall 26 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 26)
  | boardItem == Wall 27 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 27)
  | boardItem == Wall 28 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 28)
  | boardItem == Wall 29 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 29)
  | boardItem == Wall 30 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 31)
  | boardItem == Floor = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 30)
  | boardItem == Pellet NormalPellet = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 31)
  | boardItem == Pellet PowerPellet && powerPelletAnimation gs = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 30)
  | boardItem == Pellet PowerPellet && not(powerPelletAnimation gs) = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 38)
  | boardItem == TeleportBarrier = Translate (xIndex * 50) (yIndex * (-50)) (Circle 20.0)
  | boardItem == Gate = Translate (xIndex * 50) (yIndex * (-50)) (Circle 20.0)
  | boardItem == Pellet NormalPellet = Translate (xIndex * 50) (yIndex * (-50)) (Circle 20.0)
  | boardItem == Pellet PowerPellet = Translate (xIndex * 50) (yIndex * (-50)) (Circle 20.0)
  | otherwise = error "exhaustive"

-- Render the initial rows of the board
renderRows :: GameState -> Float -> Float -> Row -> [Picture] -> [Picture]
renderRows _ _ _ [] _ = []
renderRows gs yIndex xIndex [x] images = renderBoardItem x gs xIndex yIndex images : renderRows gs yIndex (xIndex + 1) [] images
renderRows gs yIndex xIndex (x:xs) images = renderBoardItem x gs xIndex yIndex images : renderRows gs yIndex (xIndex + 1) xs images

-- Render the initial board
renderBoard :: Board -> GameState -> Float -> [Picture] -> [Picture]
renderBoard [] _ _ _ = []
renderBoard [x] gs acc images = renderRows gs acc 0.0 x images ++ renderBoard [] gs (acc + 1) images  
renderBoard (x:xs) gs acc images = renderRows gs acc 0.0 x images ++ renderBoard xs gs (acc + 1) images 

-- Render the player
renderPlayer :: Player -> GameState -> [Picture] -> Picture
renderPlayer p gs images | (playerDirection p) == North && not(playerEatAnimation gs) = Translate ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 32)
                        | (playerDirection p) == North && playerEatAnimation gs = Translate ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 39)
                        | (playerDirection p) == South && not(playerEatAnimation gs) = Translate  ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 32)
                        | (playerDirection p) == South && playerEatAnimation gs = Translate  ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 40)
                        | (playerDirection p) == East && not(playerEatAnimation gs) = Translate  ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 32)
                        | (playerDirection p) == East && playerEatAnimation gs = Translate ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 41)
                        | (playerDirection p) == West && not(playerEatAnimation gs) = Translate  ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 32)
                        | (playerDirection p) == West && playerEatAnimation gs = Translate  ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 42)
                        | otherwise = error "exhaustive"

  -- renderPlayer p gs images = Translate ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 32)

-- renderPlayer p images = Translate ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 32)
                          where
                              (x,y) = playerPosition p

-- Renders all possible ghosts
renderGhosts :: GameState -> [Picture] -> [Picture]
renderGhosts gs images = map pictureGhost (ghosts gs)
                              where
                                pictureGhost ghost | theStatus == Frightened && ghostFrightenedAnimation gs = Translate ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 36)
                                                   | theStatus == Frightened && not(ghostFrightenedAnimation gs) = Translate ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 37)
                                                   | theType == Blinky = Translate ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 33)
                                                   | theType == Pinky = Translate ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 33)
                                                   | theType == Inky = Translate ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 33)
                                                   | theType == Clyde = Translate ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 34)
                                                    where
                                                      (x,y) = ghostPosition ghost
                                                      theType = ghostType ghost
                                                      theStatus = ghostStatus ghost

-- Render the current score of the player
renderScore :: Score -> Picture
renderScore score = Scale 0.2 0.2 (Color white (Translate (-900) (-1350) (Text ("Score: " ++ string))))
                    where
                        string = show score

-- Render if the game is paused as a Text
renderPause :: Bool -> Picture
renderPause pause = Scale 0.2 0.2 (Color white (Translate (-100) (-1350) (Text ("Paused: " ++ string))))
                    where
                        string = show pause                                            

-- Combines all Pictures to one IO Picture that gets rendered
render :: GameState -> [Picture] -> IO Picture
render gs images = return (pictures(renderBoard (board gs) gs 1.0 images ++ [renderPlayer (player gs) gs images] ++ renderGhosts gs images ++ [renderScore (score gs)] ++ [renderPause (paused gs)]))