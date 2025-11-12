-- src/GUI.hs
-- -----------------------------------------------------------------------------
-- Lớp giao diện người dùng bằng Gloss.
-- Chức năng:
--   - Vẽ bàn cờ, quân cờ, thông báo, nút rematch, cửa sổ chat.
--   - Bắt sự kiện chuột/phím, gửi lệnh nước đi và chat về server.
--   - Vẽ đường thắng (nếu có) theo danh sách winningCells do server cung cấp.
-- Lưu ý: giữ nguyên toàn bộ logic; chỉ bổ sung chú thích, dọn layout.
-- -----------------------------------------------------------------------------
module GUI (runGUI) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent (forkIO, MVar, newMVar, modifyMVar, readMVar, swapMVar)
import Control.Monad (unless)
import System.IO (Handle, hPutStrLn, hGetLine)
import Data.Char (toUpper, isSpace)
import Data.List (isPrefixOf, minimumBy, maximumBy)
import Data.Ord  (comparing)
import GameState (Player(..), Game(..), GameStatus(..), Cell(..), boardSize, initialGame)

-- ===== Layout =====

-- Kích thước cửa sổ và ô
windowWidth, windowHeight, cellSize :: Int
windowWidth  = 600
windowHeight = 800
cellSize     = 50

-- Vị trí-nút rematch (x, y, w, h) trong toạ độ Gloss
yesButton, noButton :: (Float, Float, Float, Float)
yesButton = (-120, -360, 140, 56)
noButton  = ( 120, -360, 140, 56)

-- ===== Client State =====
-- Trạng thái phía client cho GUI
data ClientState = ClientState
  { gameState     :: Game
  , me            :: Player
  , statusMessage :: String
  , serverHandle  :: Handle
  , chatInput     :: String
  , chatCursor    :: Int
  , chatHistory   :: [String]
  , selectAll     :: Bool
  , leftHold      :: Bool, rightHold :: Bool, backHold :: Bool
  , leftCtrlHold  :: Bool, rightCtrlHold :: Bool, backCtrlHold :: Bool
  , leftTimer     :: Float, rightTimer :: Float, backTimer :: Float
  , leftInitial   :: Bool, rightInitial :: Bool, backInitial :: Bool
  , charHold      :: Maybe Char
  , charTimer     :: Float
  , charInitial   :: Bool
  }

-- ===== Main =====
-- Khởi động GUI và vòng sự kiện Gloss
runGUI :: Handle -> Player -> IO ()
runGUI h mePlayer = do
  let st0 = ClientState
              initialGame
              mePlayer
              "Waiting for server..."
              h "" 0 [] False
              False False False
              False False False
              0 0 0
              True True True
              Nothing 0 True
  var <- newMVar st0
  _ <- forkIO (networkLoop h var)
  playIO (InWindow "CARO" (windowWidth, windowHeight) (100,100))
         black
         30
         st0
         drawState
         (handleInput var)
         (updateState var)

-- ===== Network =====
-- Nhận dòng từ server, cập nhật state và tiếp tục lặp
networkLoop :: Handle -> MVar ClientState -> IO ()
networkLoop h var = do
  msg <- hGetLine h
  mNew <- modifyMVar var $ \st -> case msg of
    "REMATCH? Y/N" -> return (st { statusMessage = "Rematch? (Y/N)" }, Nothing)
    "QUIT"         -> return (st { statusMessage = "Opponent has left." }, Nothing)
    _ | "MSG: " `isPrefixOf` msg ->
          let m = drop 5 msg in
          if m == "Invalid move." || "Welcome" `isPrefixOf` m || m == "Not your turn!" || m == "Opponent disconnected."
            then return (st { statusMessage = if m == "Invalid move." then "Invalid move!" else m }, Nothing)
            else return (st { chatHistory = take 6 (("Opponent: " ++ m) : chatHistory st) }, Nothing)
      | otherwise ->
          let g = read msg :: Game
              newMsg = case gameStatus g of
                         Running -> if currentPlayer g == me st then "Your turn!" else "Waiting for opponent..."
                         Win p   -> "Player " ++ show p ++ " wins!"
                         Draw    -> "It's a draw!"
          in return (st { gameState = g
                        , statusMessage = newMsg
                        , chatHistory = if board g == board initialGame then [] else chatHistory st
                        }, Just g)
  case mNew of
    Just g | gameStatus g /= Running -> hPutStrLn h "GAME_OVER_ACK"
    _ -> return ()
  networkLoop h var

-- ===== Tick =====
-- Lặp theo dt để xử lý giữ phím (key repeat)
updateState :: MVar ClientState -> Float -> ClientState -> IO ClientState
updateState var dt _ = do
  st <- readMVar var
  let st' = applyRepeats dt st
  _ <- swapMVar var st'
  return st'

-- ===== Draw =====
-- Vẽ toàn bộ khung hình
drawState :: ClientState -> IO Picture
drawState st = return $ Pictures
  [ drawBoard (gameState st)
  , drawWinningLine (gameState st)
  , drawMessage (statusMessage st)
  , drawRematchButtons st
  , drawChatHistory (chatHistory st)
  , drawChatInput (chatInput st) (chatCursor st) (selectAll st)
  ]

-- Thông báo: đặt ngay dưới bàn cờ, lệch xuống 24px
drawMessage :: String -> Picture
drawMessage msg =
  let c = case msg of
            "Invalid move!" -> red
            _ | "Waiting" `isPrefixOf` msg -> green
            _ | "Rematch" `isPrefixOf` msg -> green
            _ -> white
      w     = fromIntegral (boardSize * cellSize) :: Float
      half  = w / 2
      below = (80 - half) - 24
  in translate 0 below $ scale 0.2 0.2 $ color c $ Text msg

-- Lịch sử chat (tối đa 6 dòng)
drawChatHistory :: [String] -> Picture
drawChatHistory hs =
  translate (fromIntegral (- (windowWidth `div` 2) + 20))
            (fromIntegral (- (windowHeight `div` 2) + 140)) $
  Pictures $ zipWith drawLine ([0..] :: [Int]) (reverse hs)
  where
    drawLine :: Int -> String -> Picture
    drawLine i s =
      translate 0 (negate (fromIntegral i) * 25) $
      scale 0.15 0.15 $
      if "You:" `isPrefixOf` s then color cyan (Text s) else color white (Text s)

-- Ô nhập chat + con trỏ
drawChatInput :: String -> Int -> Bool -> Picture
drawChatInput input cursorPos selAll =
  let (l,r) = splitAt cursorPos input
      shown = "> " ++ l ++ "|" ++ r
      bx = fromIntegral (- (windowWidth `div` 2) + 20)
      by = fromIntegral (- (windowHeight `div` 2) + 80)
      bg = if selAll && not (null input)
             then translate (bx + 200) (by + 3)
                  $ color (makeColor 0.3 0.3 0.8 0.5) (rectangleSolid 400 28)
             else Blank
  in Pictures [ bg, translate bx by $ scale 0.2 0.2 $ color cyan $ Text shown ]

-- Nút rematch
drawRematchButtons :: ClientState -> Picture
drawRematchButtons st
  | statusMessage st == "Rematch? (Y/N)" = Pictures [yesPic, noPic]
  | otherwise = Blank
  where
    (x1,y1,w1,h1) = yesButton
    (x2,y2,w2,h2) = noButton
    yesPic = Pictures [ translate x1 y1 $ color green $ rectangleSolid w1 h1
                      , translate (x1 - w1/2 + 10) (y1 - 10) $ scale 0.2 0.2 $ color black $ Text "YES"]
    noPic  = Pictures [ translate x2 y2 $ color red   $ rectangleSolid w2 h2
                      , translate (x2 - w2/2 + 10) (y2 - 10) $ scale 0.2 0.2 $ color black $ Text "NO"]

-- Bàn cờ + quân cờ
drawBoard :: Game -> Picture
drawBoard g =
  translate 0 80 $ Pictures (gridLines ++ pieces)
  where
    b = board g
    w = fromIntegral (boardSize * cellSize)
    half = w / 2
    gridLines =
      [ Color white (Line [(x, half), (x, -half)])
        | c <- [0..boardSize], let x = fromIntegral (c * cellSize) - half ] ++
      [ Color white (Line [(-half, y), (half, y)])
        | r <- [0..boardSize], let y = fromIntegral (r * cellSize) - half ]
    pieces =
      [ translate (x + fromIntegral cellSize / 2 - half)
                  (half - y - fromIntegral cellSize / 2)
                  (drawCell (b !! r !! c))
      | r <- [0..boardSize-1], c <- [0..boardSize-1]
      , let x = fromIntegral (c * cellSize)
      , let y = fromIntegral (r * cellSize) ]

-- Vẽ đường thắng theo danh sách winningCells (0-based)
drawWinningLine :: Game -> Picture
drawWinningLine g =
  case winningCells g of
    cells | length cells < 2 -> Blank
    cells ->
      let w    = fromIntegral (boardSize * cellSize) :: Float
          half = w / 2

          -- Tâm ô -> toạ độ Gloss (0,0) ở giữa bàn
          toPt :: (Int,Int) -> (Float,Float)
          toPt (r,c) =
            ( fromIntegral (c * cellSize) + fromIntegral cellSize/2 - half
            , half - fromIntegral (r * cellSize) - fromIntegral cellSize/2 )

          rs = map fst cells
          cs = map snd cells

          allEq []     = True
          allEq (x:xs) = all (== x) xs

          -- Chọn hai đầu mút theo hướng
          (aCell, bCell)
            | allEq rs  = ((head rs, minimum cs), (head rs, maximum cs)) -- ngang
            | allEq cs  = ((minimum rs, head cs), (maximum rs, head cs)) -- dọc
            | otherwise =
                let a = minimumBy (comparing fst) cells   -- chéo
                    b = maximumBy (comparing fst) cells
                in (a,b)

          (x1,y1) = toPt aCell
          (x2,y2) = toPt bCell

          dx = x2 - x1; dy = y2 - y1
          len = sqrt (dx*dx + dy*dy)
          (ux,uy) = if len == 0 then (0,0) else (dx/len, dy/len)

          -- Kéo ra mép ô đầu/cuối
          extend = fromIntegral cellSize / 2
          (x1e,y1e) = (x1 - ux*extend, y1 - uy*extend)
          (x2e,y2e) = (x2 + ux*extend, y2 + uy*extend)

          -- Thick line = hình tứ giác mỏng
          thick = 12.0
          (nx,ny) = (-uy * thick/2, ux * thick/2)
          poly = Polygon [ (x1e - nx, y1e - ny)
                         , (x1e + nx, y1e + ny)
                         , (x2e + nx, y2e + ny)
                         , (x2e - nx, y2e - ny) ]
      in translate 0 80 $ Color cyan poly

-- Vẽ quân
drawCell :: Cell -> Picture
drawCell Empty     = Blank
drawCell (Taken X) = drawX
drawCell (Taken O) = drawO

drawO :: Picture
drawO = Color blue $ ThickCircle (fromIntegral cellSize * 0.4) 5

drawX :: Picture
drawX = Color red $ Pictures
  [ Rotate 45    (Polygon [(-l,-t/2),(l,-t/2),(l,t/2),(-l,t/2)])
  , Rotate (-45) (Polygon [(-l,-t/2),(l,-t/2),(l,t/2),(-l,t/2)])
  ]
  where l = fromIntegral cellSize * 0.45
        t = 8.0

-- ===== Hit-test =====
-- Kiểm tra click nằm trong hình chữ nhật tâm (rx,ry), kích thước (rw,rh)
isClickInRect :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
isClickInRect (cx, cy) (rx, ry, rw, rh) =
  cx >= rx - rw / 2 && cx <= rx + rw / 2 &&
  cy >= ry - rh / 2 && cy <= ry + rh / 2

-- Chuyển toạ độ màn hình -> (row,col) trên bàn (0-based)
pixelToGrid :: (Float, Float) -> Maybe (Int, Int)
pixelToGrid (mx, my) =
  let w = fromIntegral (boardSize * cellSize)
      half = w / 2
      topY = 80 + half   -- dịch bàn lên 80px
      leftX = -half
      cx = mx - leftX
      cy = topY - my
  in if cx < 0 || cx > w || cy < 0 || cy > w
       then Nothing
       else let r = floor $ cy / fromIntegral cellSize
                c = floor $ cx / fromIntegral cellSize
            in if r >= 0 && r < boardSize && c >= 0 && c < boardSize then Just (r,c) else Nothing

-- ===== Input =====
-- Xử lý chuột/phím. Giữ nguyên thứ tự pattern như bản gốc.

-- Click chuột trái: đánh cờ hoặc bấm rematch
handleInput :: MVar ClientState -> Event -> ClientState -> IO ClientState
handleInput var (EventKey (MouseButton LeftButton) Down _ (mx, my)) st
  | statusMessage st == "Rematch? (Y/N)" && isClickInRect (mx, my) yesButton =
      hPutStrLn (serverHandle st) "Y" >> swapMVar var (st { statusMessage = "Waiting for opponent..." }) >> return (st { statusMessage = "Waiting for opponent..." })
  | statusMessage st == "Rematch? (Y/N)" && isClickInRect (mx, my) noButton  =
      hPutStrLn (serverHandle st) "N" >> swapMVar var (st { statusMessage = "Quitting." }) >> return (st { statusMessage = "Quitting." })
  | currentPlayer (gameState st) == me st && gameStatus (gameState st) == Running =
      case pixelToGrid (mx, my) of
        Just (r,c) -> hPutStrLn (serverHandle st) (show (r+1) ++ " " ++ show (c+1)) >> return st
        Nothing    -> return st
  | otherwise = return st

-- Enter: gửi chat
handleInput var (EventKey (SpecialKey KeyEnter) Down _ _) st = do
  let s = chatInput st
  unless (null s) $ hPutStrLn (serverHandle st) ("CHAT:" ++ s)
  let hist' = if null s then chatHistory st else take 6 (("You: " ++ s) : chatHistory st)
      st' = st { chatInput = "", chatCursor = 0, chatHistory = hist' }
  _ <- swapMVar var st'
  return st'

-- Backspace (nhấn-giữ, Ctrl xoá theo từ)
handleInput var (EventKey (SpecialKey KeyBackspace) Down (Modifiers _ ctrl _) _) st
  | statusMessage st == "Rematch? (Y/N)" = return st
  | selectAll st =
      let st' = st { chatInput = "", chatCursor = 0, selectAll = False
                   , backHold = True, backCtrlHold = (ctrl==Down), backTimer = 0, backInitial = True }
      in swapMVar var st' >> return st'
  | ctrl == Down =
      let (s', i') = deletePrevWordAt (chatInput st) (chatCursor st)
          st' = st { chatInput = s', chatCursor = i'
                   , backHold = True, backCtrlHold = True, backTimer = 0, backInitial = True }
      in swapMVar var st' >> return st'
  | otherwise =
      let (s', i') = backspaceAt (chatInput st) (chatCursor st)
          st' = st { chatInput = s', chatCursor = i'
                   , backHold = True, backCtrlHold = False, backTimer = 0, backInitial = True }
      in swapMVar var st' >> return st'

handleInput var (EventKey (SpecialKey KeyBackspace) Up _ _) st =
  swapMVar var (st { backHold = False }) >> return (st { backHold = False })

-- Mũi tên trái/phải (nhấn-giữ, Ctrl nhảy theo từ)
handleInput var (EventKey (SpecialKey KeyLeft) Down (Modifiers _ ctrl _) _) st
  | statusMessage st == "Rematch? (Y/N)" = return st
  | otherwise =
      let s = chatInput st; i = chatCursor st
          i' = if ctrl == Down then moveWordLeft s i else max 0 (i - 1)
          st' = st { chatCursor = i', selectAll = False
                   , leftHold = True, leftCtrlHold = (ctrl==Down), leftTimer = 0, leftInitial = True }
      in swapMVar var st' >> return st'

handleInput var (EventKey (SpecialKey KeyLeft) Up _ _) st =
  swapMVar var (st { leftHold = False }) >> return (st { leftHold = False })

handleInput var (EventKey (SpecialKey KeyRight) Down (Modifiers _ ctrl _) _) st
  | statusMessage st == "Rematch? (Y/N)" = return st
  | otherwise =
      let s = chatInput st; i = chatCursor st; l = length s
          i' = if ctrl == Down then moveWordRight s i else min l (i + 1)
          st' = st { chatCursor = i', selectAll = False
                   , rightHold = True, rightCtrlHold = (ctrl==Down), rightTimer = 0, rightInitial = True }
      in swapMVar var st' >> return st'

handleInput var (EventKey (SpecialKey KeyRight) Up _ _) st =
  swapMVar var (st { rightHold = False }) >> return (st { rightHold = False })

-- Space (nhấn-giữ để chèn liên tục)
handleInput var (EventKey (SpecialKey KeySpace) Down _ _) st
  | statusMessage st == "Rematch? (Y/N)" = return st
  | otherwise =
      let baseS = if selectAll st then "" else chatInput st
          baseI = if selectAll st then 0  else chatCursor st
          (s', i') = insertCharAt ' ' baseS baseI
          st' = st { chatInput = s', chatCursor = i', selectAll = False
                   , charHold = Just ' ', charTimer = 0, charInitial = True }
      in swapMVar var st' >> return st'

handleInput var (EventKey (SpecialKey KeySpace) Up _ _) st =
  swapMVar var (st { charHold = Nothing, charTimer = 0, charInitial = True }) >>
  return (st { charHold = Nothing, charTimer = 0, charInitial = True })

-- Fallback cho phím xoá phát sinh dạng Char (tuỳ layout)
handleInput var (EventKey (Char '\b')  Down (Modifiers _ ctrl _) _) st =
  handleInput var (EventKey (SpecialKey KeyBackspace) Down (Modifiers Up ctrl Up) (0,0)) st
handleInput var (EventKey (Char '\b')  Up   _                         _) st =
  handleInput var (EventKey (SpecialKey KeyBackspace) Up   (Modifiers Up Up  Up) (0,0)) st
handleInput var (EventKey (Char '\DEL') Down (Modifiers _ ctrl _) _) st =
  handleInput var (EventKey (SpecialKey KeyBackspace) Down (Modifiers Up ctrl Up) (0,0)) st
handleInput var (EventKey (Char '\DEL') Up   _                         _) st =
  handleInput var (EventKey (SpecialKey KeyBackspace) Up   (Modifiers Up Up  Up) (0,0)) st

-- Gõ ký tự thường + Y/N ở màn rematch + giữ ký tự để lặp
handleInput var (EventKey (Char c) Down _ _) st
  | statusMessage st == "Rematch? (Y/N)" =
      case toUpper c of
        'Y' -> hPutStrLn (serverHandle st) "Y" >> swapMVar var (st { statusMessage = "Waiting for opponent..." }) >> return (st { statusMessage = "Waiting for opponent..." })
        'N' -> hPutStrLn (serverHandle st) "N" >> swapMVar var (st { statusMessage = "Quitting." }) >> return (st { statusMessage = "Quitting." })
        _   -> return st
  | c >= ' ' =
      let baseS = if selectAll st then "" else chatInput st
          baseI = if selectAll st then 0  else chatCursor st
          (s', i') = insertCharAt c baseS baseI
          st' = st { chatInput = s', chatCursor = i', selectAll = False
                   , charHold = Just c, charTimer = 0, charInitial = True }
      in swapMVar var st' >> return st'
  | otherwise = return st

handleInput var (EventKey (Char _) Up _ _) st =
  swapMVar var (st { charHold = Nothing, charTimer = 0, charInitial = True }) >>
  return (st { charHold = Nothing, charTimer = 0, charInitial = True })

-- Mặc định: không làm gì
handleInput _ _ st = return st

-- ===== Text edit helpers =====
-- Chèn/xoá ký tự, nhảy theo từ, xoá theo từ

insertCharAt :: Char -> String -> Int -> (String, Int)
insertCharAt c s i = let (l,r) = splitAt i s in (l ++ [c] ++ r, i+1)

backspaceAt :: String -> Int -> (String, Int)
backspaceAt s i | i <= 0    = (s, 0)
                | otherwise = let (l,r) = splitAt i s in (init l ++ r, i-1)

deletePrevWordAt :: String -> Int -> (String, Int)
deletePrevWordAt s i =
  let (l,r) = splitAt i s
      dropSpaces = reverse . dropWhile isSpace . reverse
      dropWord t = reverse . dropWhile (not . isSpace) . reverse $ t
      l' = dropSpaces (dropWord (dropSpaces l))
  in (l' ++ r, length l')

moveWordLeft :: String -> Int -> Int
moveWordLeft s i =
  let l = take i s
      goSpaces = reverse . dropWhile isSpace . reverse
      goWord   = reverse . dropWhile (not . isSpace) . reverse
  in length (goWord (goSpaces l))

moveWordRight :: String -> Int -> Int
moveWordRight s i =
  let r = drop i s
      r' = dropWhile (not . isSpace) r
      r''= dropWhile isSpace r'
  in length s - length r''

-- ===== Key repeat =====
-- Tham số nhấn-giữ (delay ban đầu, tần số lặp)
initialDelay, repeatRate :: Float
initialDelay = 0.35
repeatRate   = 0.05

-- Áp dụng nhấn-giữ cho mũi tên trái/phải, backspace, và ký tự thường
applyRepeats :: Float -> ClientState -> ClientState
applyRepeats dt st =
  let (st1, _) =
        if leftHold st
          then let t = leftTimer st + dt; initP = leftInitial st in
               if (initP && t >= initialDelay) || (not initP && t >= repeatRate)
                 then let s = chatInput st; i = chatCursor st
                          i' = if leftCtrlHold st then moveWordLeft s i else max 0 (i - 1)
                      in (st { chatCursor = i', selectAll = False, leftTimer = 0, leftInitial = False }, True)
                 else (st { leftTimer = t }, False)
          else (st { leftTimer = 0, leftInitial = True }, False)
      (st2, _) =
        if rightHold st1
          then let t = rightTimer st1 + dt; initP = rightInitial st1 in
               if (initP && t >= initialDelay) || (not initP && t >= repeatRate)
                 then let s = chatInput st1; i = chatCursor st1; l = length s
                          i' = if rightCtrlHold st1 then moveWordRight s i else min l (i + 1)
                      in (st1 { chatCursor = i', selectAll = False, rightTimer = 0, rightInitial = False }, True)
                 else (st1 { rightTimer = t }, False)
          else (st1 { rightTimer = 0, rightInitial = True }, False)
      st3 =
        if backHold st2
          then let t = backTimer st2 + dt; initP = backInitial st2 in
               if (initP && t >= initialDelay) || (not initP && t >= repeatRate)
                 then let s = chatInput st2; i = chatCursor st2
                          (s', i') = if backCtrlHold st2 then deletePrevWordAt s i else backspaceAt s i
                      in st2 { chatInput = s', chatCursor = i', selectAll = False, backTimer = 0, backInitial = False }
                 else st2 { backTimer = t }
          else st2 { backTimer = 0, backInitial = True }
      st4 =
        case charHold st3 of
          Nothing -> st3 { charTimer = 0, charInitial = True }
          Just ch ->
            if statusMessage st3 == "Rematch? (Y/N)"
               then st3 { charHold = Nothing, charTimer = 0, charInitial = True }
               else
                 let t = charTimer st3 + dt
                     initP = charInitial st3
                 in if (initP && t >= initialDelay) || (not initP && t >= repeatRate)
                      then let baseS = chatInput st3
                               baseI = chatCursor st3
                               (s', i') = insertCharAt ch baseS baseI
                           in st3 { chatInput = s', chatCursor = i', selectAll = False
                                   , charTimer = 0, charInitial = False }
                      else st3 { charTimer = t }
  in st4
