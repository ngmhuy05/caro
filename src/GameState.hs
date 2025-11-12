-- src/GameState.hs
-- -----------------------------------------------------------------------------
-- Mô hình trạng thái trò chơi (thuần dữ liệu).
-- Không đổi bất kỳ giá trị/hàm nào.
-- -----------------------------------------------------------------------------
module GameState where

-- Kích thước bàn và điều kiện thắng
boardSize :: Int
boardSize = 10

winCondition :: Int
winCondition = 5

-- Người chơi và ô cờ
data Player = X | O deriving (Eq, Show, Read)
data Cell   = Empty | Taken Player deriving (Eq, Show, Read)
type Board  = [[Cell]]

-- Trạng thái ván
data GameStatus = Running | Win Player | Draw deriving (Eq, Show, Read)

-- Trạng thái tổng thể
data Game = Game
  { board         :: Board              -- ma trận ô cờ
  , currentPlayer :: Player             -- ai đi tiếp
  , gameStatus    :: GameStatus         -- đang chơi / thắng / hoà
  , winningCells  :: [(Int, Int)]       -- dãy ô thắng (0-based), có thể rỗng
  } deriving (Show, Read)

-- Khởi tạo ván mới
initialGame :: Game
initialGame = Game
  { board = replicate boardSize (replicate boardSize Empty)
  , currentPlayer = X
  , gameStatus = Running
  , winningCells = []
  }

-- Đổi lượt
nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X
