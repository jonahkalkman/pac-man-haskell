module View where

import Graphics.Gloss
import Model

renderBoardItem :: BoardItem -> Float -> Float -> [Picture] -> Picture
renderBoardItem boardItem xIndex yIndex images
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



  | boardItem == Floor = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 30)
  | boardItem == Pellet NormalPellet = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 295) (images !! 31)


  | boardItem == TeleportBarrier = Translate (xIndex * 50) (yIndex * (-50)) (Circle 20.0)
  | boardItem == Gate = Translate (xIndex * 50) (yIndex * (-50)) (Circle 20.0)
  | boardItem == Pellet NormalPellet = Translate (xIndex * 50) (yIndex * (-50)) (Circle 20.0)
  | boardItem == Pellet PowerPellet = Translate (xIndex * 50) (yIndex * (-50)) (Circle 20.0)
  | boardItem == Fruit Cherry 200 = Translate (xIndex * 50) (yIndex * (-50)) (Circle 20.0)
  | boardItem == Fruit StrawBerry 300 = Translate (xIndex * 50) (yIndex * (-50)) (Circle 20.0)
  | boardItem == Fruit Orange 400 = Translate (xIndex * 50) (yIndex * (-50)) (Circle 20.0)
  | boardItem == Fruit Apple 500 = Translate (xIndex * 50) (yIndex * (-50)) (Circle 20.0)
  | boardItem == Fruit Melon 600 = Translate (xIndex * 50) (yIndex * (-50)) (Circle 20.0)
  | boardItem == Fruit GalaxianFlagship 700 = Translate (xIndex * 50) (yIndex * (-50)) (Circle 20.0)
  | boardItem == Fruit Bell 800 = Translate (xIndex * 50) (yIndex * (-50)) (Circle 20.0)
  | boardItem == Fruit Key 900 = Translate (xIndex * 50) (yIndex * (-50)) (Circle 20.0)
  | otherwise = error "exhaustive"

renderRows :: Float -> Float -> Row -> [Picture] -> [Picture]
renderRows _ _ [] _ = []
renderRows yIndex xIndex [x] images = renderBoardItem x xIndex yIndex images : renderRows yIndex (xIndex + 1) [] images
renderRows yIndex xIndex (x:xs) images = renderBoardItem x xIndex yIndex images : renderRows yIndex (xIndex + 1) xs images

renderBoard :: Board -> Float -> [Picture] -> [Picture]
renderBoard [] _ _ = []
renderBoard [x] acc images = renderRows acc 0.0 x images ++ renderBoard [] (acc + 1) images  
renderBoard (x:xs) acc images = renderRows acc 0.0 x images ++ renderBoard xs (acc + 1) images                  

render :: GameState -> [Picture] -> Picture
render gs images = pictures(renderBoard (board gs) 1.0 images ++ [renderPlayer (player gs) images] ++ [renderPosition (player gs)] ++ renderGhosts (ghosts gs) images ++ [renderScore (ghosts gs)])

renderPlayer :: Player -> [Picture] -> Picture
renderPlayer p images = Translate ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 32)
                          where
                              (x,y) = playerPosition p
-- Blinky | Pinky | Inky | Clyde
renderGhosts :: Ghosts -> [Picture] -> [Picture]
renderGhosts ghosts images = map pictureGhost ghosts
                              where
                                pictureGhost ghost | theType == Blinky = Translate ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 33)
                                                   | theType == Pinky = Translate ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 33)
                                                   | theType == Inky = Translate ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 33)
                                                   | theType == Clyde = Translate ((x * 16) - 224 + 8) (((y + 1) * (-16)) + 295) (images !! 33)
                                                    where
                                                      (x,y) = ghostPosition ghost
                                                      theType = ghostType ghost

renderPosition :: Player -> Picture
renderPosition p = Color white (Translate (-100) 100 (Text string))
                    where
                        string = show x ++ "/" ++ show y
                        (x,y) = playerPosition p

renderScore :: Ghosts -> Picture
renderScore ghosts = Color white (Translate (-100) 0 (Text ("Score:" ++ string)))
                    where
                        string = show (ghostDirection (head ghosts))

roundpos (x,y) = (realToFrac(round x), realToFrac(round y))