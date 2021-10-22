module View where

import Graphics.Gloss
import Model

renderBoardItem :: BoardItem -> Float -> Float -> Picture
renderBoardItem boardItem xIndex yIndex 
  | boardItem == Wall 1 = Translate (xIndex * 50) (yIndex * 50) (ThickCircle 20.0 20.0)
  | boardItem == Floor = Translate (xIndex * 50) (yIndex * 50) (Circle 20.0)

renderRows :: Float -> Float -> Row -> [Picture]
renderRows _ _ [] = []
renderRows yIndex xIndex [x] = renderBoardItem x xIndex yIndex : renderRows yIndex (xIndex + 1) []
renderRows yIndex xIndex (x:xs) = renderBoardItem x xIndex yIndex : renderRows yIndex (xIndex + 1) xs

renderBoard :: Board -> Float -> [Picture]
renderBoard [] _ = []
renderBoard [x] acc = renderRows acc 0.0 x ++ renderBoard [] (acc + 1)   
renderBoard (x:xs) acc = renderRows acc 0.0 x ++ renderBoard xs (acc + 1)                   

viewPure :: GameState -> Picture
viewPure = render


view :: GameState -> IO Picture
view = return . viewPure

render gs = pictures(renderBoard (board gs) 1.0)