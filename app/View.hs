module View where

import Graphics.Gloss
import Model

renderBoardItem :: BoardItem -> Float -> Float -> [Picture] -> Picture
renderBoardItem boardItem xIndex yIndex images
  | boardItem == Wall 1 = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 224) (head images)
  | boardItem == Floor = Translate ((xIndex * 16) - 224 + 8) ((yIndex * (-16)) + 224) (rectangleSolid 16 16)
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

renderRows :: Float -> Float -> Row -> [Picture] -> [Picture]
renderRows _ _ [] _ = []
renderRows yIndex xIndex [x] images = renderBoardItem x xIndex yIndex images : renderRows yIndex (xIndex + 1) [] images
renderRows yIndex xIndex (x:xs) images = renderBoardItem x xIndex yIndex images : renderRows yIndex (xIndex + 1) xs images

renderBoard :: Board -> Float -> [Picture] -> [Picture]
renderBoard [] _ _ = []
renderBoard [x] acc images = renderRows acc 0.0 x images ++ renderBoard [] (acc + 1) images  
renderBoard (x:xs) acc images = renderRows acc 0.0 x images ++ renderBoard xs (acc + 1) images                  

render :: GameState -> [Picture] -> Picture
render gs images = pictures(renderPlayer (player gs) images : renderBoard (board gs) 1.0 images)

renderPlayer :: Player -> [Picture] -> Picture
renderPlayer p images = Translate xIndex yIndex (head (tail images))
                          where
                              xIndex = fst (playerPosition p)
                              yIndex = snd (playerPosition p)