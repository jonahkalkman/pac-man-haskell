data GameState = GameState {
  board :: Board,
  player :: Player,
  ghosts :: Ghosts,
  score :: Score,
  paused :: Bool,
  lives :: Int,
  gameLevel :: Int
}

newtype Board = Board [[BoardItem]]

type Score = int
type Position = Point
type Velocity = Float
data Direction = North | East | South | West

data Player = P {
  playerStatus :: PlayerStatus,
  playerSpeed :: Velocity,
  playerPosition :: Position,
  playerDirection :: Direction
}

data PlayerStatus = Alive | Dead

data BoardItem = Player Player | Ghost Ghost | Wall WallType | Floor | TeleportBarrier | Gate | Pellet PelletType | Fruit FruitType FruitPoints
type WallType = Int
data PelletType = NormalPellet | PowerPellet
data FruitType = Cherry | StrawBerry | Orange | Apple | Melon | GalaxianFlagship | Bell | Key
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

