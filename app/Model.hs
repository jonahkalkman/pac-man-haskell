module Model where

import Graphics.Gloss.Data.Point

data GameState = GameState {
  board :: Board,
  player :: Player
  -- ghosts :: Ghosts,
  -- score :: Score,
  -- paused :: Bool,
  -- lives :: Int,
  -- gameLevel :: Int
}

type Row = [BoardItem]
type Board = [Row]

type Score = Int
type Position = Point
type Velocity = Float
data Direction = North | East | South | West | None
                  deriving Eq

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

data GhostType = Blinky | Pinky | Inky | Clyde
data GhostStatus = Scatter | Frightened | Eaten | Chase

initialBoard :: Board
initialBoard = [[Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor],[Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor, Floor],[Wall 12, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 16, Wall 17, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 11, Wall 13],[Wall 8, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Pellet NormalPellet, Wall 9],[Wall 8, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 1, Wall 0, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 6, Wall 2, Wall 2, Wall 7, Pellet NormalPellet, Wall 9],[Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1],[Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1],[Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1],[Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1],[Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1],[Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1],[Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1],[Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1],[Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1],[Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1],[Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1, Wall 1]]
player2 :: Player
player2 = P Alive 1.0 (0,0) North

initialState :: GameState
initialState = GameState initialBoard player2