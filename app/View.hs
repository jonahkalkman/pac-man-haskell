module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

render = pictures [pic1 pic2]
pic1 = Translate (-190) 190 (ThickCircle 8.0 2.0)
pic2 = Translate (-170) 170 (Text "pic2")

-- Voor alle posities krijg alle boardItems, getRows [(Int,Int)] -> Board -> [BoardItem]
-- [[wall], [wall]] -> [wall,wall] -> concat : concat b = [BoardItem]
getBoardItem :: (Int,Int) -> Board -> BoardItem
-- getBoardItem [] b = []
getBoardItem ((x, y):rest) b = (b!!y) !! x

-- for each row render items
-- renderItem :: Board -> Picture
-- renderItem b rowIndex = map (renderItem (rowIndex + 1)) b 

-- Static oplossing
values = [ (x,y) | x <- [0..3],
         y <- [0..2] ]

-- x = -200 + (20*x)
-- y = 

-- calcBoardPos :: [(Int,Int)] -> [BoardItem] -> Picture
-- calcBoardPos ((x, y):rest) (a:as) =
--     | a == Wall 1 = Translate xcords ycords (ThickCircle 8.0 2.0)
--     | a == Floor = Translate xcords ycords (Text "floor")
--   where
--    xcords = -200 + (20*x)
--    ycords = -200 + (20*y)

    
  


  -- for each boardItem do this:
  -- index van x is index in row 
  -- index van y is index in board (row)
                    
    
    
--     voor elk item in lists add +1 to y, voor elk item in list of lists add +1 to x
-- daarvan getboarditem (x,y) board

viewPure :: GameState -> Picture
-- viewPure gstate = Pictures [pic1,pic2]
viewPure gstate = render

  -- case getBoardItem (0,0) (board gstate) of
  --   Wall 1 -> pic1
  --   Floor -> pic2 


-- viewPure :: GameState -> Picture
-- viewPure gstate = case infoToShow gstate of
--   ShowNothing   -> blank
--   ShowANumber n -> color green (text (show n))
--   ShowAChar   c -> color green (text [c])