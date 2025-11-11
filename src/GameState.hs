-- File: GameState.hs
module GameState where

boardSize :: Int
boardSize = 10

winCondition :: Int
winCondition = 5

data Player = X | O deriving (Eq, Show, Read)
data Cell = Empty | Taken Player deriving (Eq, Show, Read)
type Board = [[Cell]]

data GameStatus = Running | Win Player | Draw deriving (Eq, Show, Read)

data Game = Game
  { board         :: Board
  , currentPlayer :: Player
  , gameStatus    :: GameStatus
  , winningCells  :: [(Int, Int)]
  } deriving (Show, Read)

initialGame :: Game
initialGame = Game
  { board = replicate boardSize (replicate boardSize Empty)
  , currentPlayer = X
  , gameStatus = Running
  , winningCells = []
  }

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X
