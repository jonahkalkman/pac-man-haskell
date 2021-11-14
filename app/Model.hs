module Model where

import Graphics.Gloss.Data.Point

data GameState = GameState {
  board :: Board,
  player :: Player,
  ghosts :: Ghosts,
  score :: Score,
  paused :: Bool,
  ghostFrightenedAnimation :: Bool,
  playerEatAnimation :: Bool
}

type Row = [BoardItem]
type Board = [Row]

type Score = Int
type Position = Point
type Velocity = Float
data Direction = North | East | South | West | None
                  deriving (Eq, Show)

data Player = P {
  playerStatus :: PlayerStatus,
  playerSpeed :: Velocity,
  playerPosition :: Position,
  playerDirection :: Direction
}

data PlayerStatus = Alive | Dead
data BoardItem = Wall WallType | Floor | TeleportBarrier | Gate | Pellet PelletType | Fruit FruitType FruitPoints
                  deriving(Eq)    

type WallType = Int
data PelletType = NormalPellet | PowerPellet
                    deriving(Eq)
data FruitType = Cherry | StrawBerry | Orange | Apple | Melon | GalaxianFlagship | Bell | Key
                  deriving(Eq) 
type FruitPoints = Int

type Ghosts = [Ghost]
data Ghost = G {
  ghostType :: GhostType,
  ghostStatus :: GhostStatus,
  ghostSpeed :: Velocity,
  ghostPosition :: Position,
  ghostDirection :: Direction
}

data GhostType = Blinky | Pinky | Inky | Clyde deriving(Eq) 
data GhostStatus = Scatter | Frightened | Eaten | Chase deriving(Eq, Show) 

initialBoard :: Board
initialBoard = [
  [Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor],
  [Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor],
  [Wall 12, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 16, Wall 17, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 13],
  [Wall 8, Pellet PowerPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 9],
  [Wall 8, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 9],
  [Wall 8, Floor, Wall 1, Floor, Floor, Wall 0, Pellet NormalPellet, Wall 1, Floor, Floor, Floor, Wall 0, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Wall 1, Floor, Floor, Floor, Wall 0, Pellet NormalPellet, Wall 1, Floor, Floor, Wall 0, Floor, Wall 9],
  [Wall 8, Pellet NormalPellet, Wall 5, Wall 3, Wall 3, Wall 4, Pellet NormalPellet, Wall 5, Wall 3, Wall 3, Wall 3, Wall 4, Pellet NormalPellet, Wall 5, Wall 4, Pellet NormalPellet, Wall 5, Wall 3, Wall 3, Wall 3, Wall 4, Pellet NormalPellet, Wall 5, Wall 3, Wall 3, Wall 4, Pellet NormalPellet, Wall 9],
  [Wall 8, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 9],
  [Wall 8, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 6, Wall 7, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 2, Wall 2, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 6, Wall 7, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 9],
  [Wall 8, Pellet NormalPellet, Wall 5, Wall 3, Wall 3, Wall 4, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Wall 5, Wall 3, Wall 3, Wall 29, Wall 28, Wall 3, Wall 3, Wall 4, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Wall 5, Wall 3, Wall 3, Wall 4, Pellet NormalPellet, Wall 9],
  [Wall 8, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet,Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 9],
  [Wall 14, Wall 10, Wall 10, Wall 10, Wall 10, Wall 7, Pellet NormalPellet, Wall 1, Wall 5, Wall 2, Wall 2, Wall 7, Floor, Wall 1, Wall 0, Floor, Wall 6, Wall 2, Wall 2, Wall 4, Wall 0, Pellet NormalPellet, Wall 6, Wall 10, Wall 10, Wall 10, Wall 10, Wall 15],
  [Floor, Floor, Floor, Floor, Floor, Wall 8, Pellet NormalPellet, Wall 1, Wall 6, Wall 3, Wall 3, Wall 4, Floor, Wall 5, Wall 4, Floor, Wall 5, Wall 3, Wall 3, Wall 7, Wall 0, Pellet NormalPellet, Wall 9, Floor, Floor, Floor, Floor, Floor],
  [Floor, Floor, Floor, Floor, Floor, Wall 8, Pellet NormalPellet, Wall 1, Wall 0, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall 1, Wall 0, Pellet NormalPellet, Wall 9, Floor, Floor, Floor, Floor, Floor],
  [Floor, Floor, Floor, Floor, Floor, Wall 8, Pellet NormalPellet, Wall 1, Wall 0, Floor, Wall 20, Wall 10, Wall 24, Floor, Floor, Wall 25, Wall 10, Wall 21, Floor, Wall 1, Wall 0, Pellet NormalPellet, Wall 9, Floor, Floor, Floor, Floor, Floor],
  [Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 4, Pellet NormalPellet, Wall 5, Wall 4, Floor, Wall 8, Floor, Floor, Floor, Floor, Floor, Floor, Wall 9, Floor, Wall 5, Wall 4, Pellet NormalPellet, Wall 5, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11],
  [Floor, Floor, Floor, Floor, Floor, Floor, Pellet NormalPellet, Floor, Floor, Floor, Wall 8, Floor, Floor, Floor, Floor, Floor, Floor, Wall 9, Floor, Floor, Floor, Pellet NormalPellet, Floor, Floor, Floor, Floor, Floor, Floor],
  [Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 7, Pellet NormalPellet, Wall 6, Wall 7, Floor, Wall 8, Floor, Floor, Floor, Floor, Floor, Floor, Wall 9, Floor, Wall 6, Wall 7, Pellet NormalPellet, Wall 6, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10],
  [Floor, Floor, Floor, Floor, Floor, Wall 8, Pellet NormalPellet, Wall 1, Wall 0, Floor, Wall 22, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 23, Floor, Wall 1, Wall 0, Pellet NormalPellet, Wall 9, Floor, Floor, Floor, Floor, Floor],
  [Floor, Floor, Floor, Floor, Floor, Wall 8, Pellet NormalPellet, Wall 1, Wall 0, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Wall 1, Wall 0, Pellet NormalPellet, Wall 9, Floor, Floor, Floor, Floor, Floor],
  [Floor, Floor, Floor, Floor, Floor, Wall 8, Pellet NormalPellet, Wall 1, Wall 0, Floor, Wall 6, Wall 2, Wall 2, Wall 2, Wall 2, Wall 2, Wall 2, Wall 7, Floor, Wall 1, Wall 0, Pellet NormalPellet, Wall 9, Floor, Floor, Floor, Floor, Floor],
  [Wall 12, Wall 11, Wall 11, Wall 11, Wall 11, Wall 4, Pellet NormalPellet, Wall 5, Wall 4, Floor, Wall 5, Wall 3, Wall 3, Wall 29, Wall 28, Wall 3, Wall 3, Wall 4, Floor, Wall 5, Wall 4, Pellet NormalPellet, Wall 5, Wall 11, Wall 11, Wall 11, Wall 11, Wall 13],
  [Wall 8, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 9],
  [Wall 8, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 9],
  [Wall 8, Pellet NormalPellet, Wall 5, Wall 3, Wall 7, Wall 0, Pellet NormalPellet, Wall 5, Wall 3, Wall 3, Wall 3, Wall 4, Pellet NormalPellet, Wall 5, Wall 4, Pellet NormalPellet, Wall 5, Wall 3, Wall 3, Wall 3, Wall 4, Pellet NormalPellet, Wall 1, Wall 6, Wall 3, Wall 4, Pellet NormalPellet, Wall 9],
  [Wall 8, Floor, Pellet NormalPellet, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Floor, Floor, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Pellet NormalPellet, Floor, Wall 9],
  [Wall 18, Wall 2, Wall 7, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Wall 6, Wall 7, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 2, Wall 2, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 6, Wall 7, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Wall 6, Wall 2, Wall 19],
  [Wall 17, Wall 3, Wall 4, Pellet NormalPellet, Wall 5, Wall 4, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Wall 5, Wall 3, Wall 3, Wall 7, Wall 6, Wall 3, Wall 3, Wall 4, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Wall 5, Wall 4, Pellet NormalPellet, Wall 5, Wall 3, Wall 16],
  [Wall 8, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 9],
  [Wall 8, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 2, Wall 2, Wall 4, Wall 5, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 4, Wall 5, Wall 2, Wall 2, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 9],
  [Wall 8, Pellet NormalPellet, Wall 5, Wall 3, Wall 3, Wall 3, Wall 3, Wall 3, Wall 3, Wall 3, Wall 3, Wall 4, Pellet NormalPellet, Wall 5, Wall 4, Pellet NormalPellet, Wall 5, Wall 3, Wall 3, Wall 3, Wall 3, Wall 3, Wall 3, Wall 3, Wall 3, Wall 4, Pellet NormalPellet, Wall 9],
  [Wall 8, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 9],
  [Wall 14, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 10, Wall 15],
  [Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor],
  [Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor]
  ]
player2 :: Player  
player2 = P Alive 1.0 (26,8) East

initialGhosts :: [Ghost]
initialGhosts = [G Blinky Scatter 1.0 (12,14) North, G Blinky Scatter 1.0 (12,10) North]

initialState :: GameState
initialState = GameState initialBoard player2 initialGhosts 0 False False False