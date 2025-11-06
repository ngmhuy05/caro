-- File: GUI.hs
module GUI (
    runGUI
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent (forkIO, MVar, newMVar, modifyMVar, readMVar, swapMVar)
import Control.Monad (unless)
import System.IO (Handle, hPutStrLn, hGetLine)
import Data.Char (toUpper, isSpace)
import Data.List (isPrefixOf, maximumBy)
import Data.Ord (comparing)

import GameState (Player(..), Game(..), GameStatus(..), Cell(..), Board, boardSize, initialGame, winCondition)

-- Window dimensions and constants
windowWidth, windowHeight, cellSize :: Int
padding :: Float
windowWidth = 600
windowHeight = 800
cellSize = 50
padding = (fromIntegral windowWidth - fromIntegral boardSize * fromIntegral cellSize) / 2

-- Rematch buttons
yesButton, noButton :: (Float, Float, Float, Float) -- (x, y, width, height)
yesButton = (-100, -150, 120, 50)
noButton  = (100, -150, 120, 50)

-- Client internal state
data ClientState = ClientState {
    gameState     :: Game,
    me            :: Player,
    statusMessage :: String,
    serverHandle  :: Handle,
    chatInput     :: String,
    chatCursor    :: Int,
    chatHistory   :: [String],
    selectAll    :: Bool,
    leftHold     :: Bool,
    rightHold    :: Bool,
    backHold     :: Bool,
    leftCtrlHold :: Bool,
    rightCtrlHold:: Bool,
    backCtrlHold :: Bool,
    leftTimer    :: Float,
    rightTimer   :: Float,
    backTimer    :: Float,
    leftInitial  :: Bool,
    rightInitial :: Bool,
    backInitial  :: Bool
}

-- === MAIN GUI FUNCTION ===
runGUI :: Handle -> Player -> IO ()
runGUI h mePlayer = do
    let initialState = ClientState initialGame mePlayer "Waiting for server..." h "" 0 [] False False False False False False False 0 0 0 True True True
    stateVar <- newMVar initialState
    -- network thread
    _ <- forkIO (networkLoop h stateVar)
    -- Gloss loop
    playIO
        (InWindow "Gomoku" (windowWidth, windowHeight) (100, 100))
        black
        30
        initialState
        drawState
        (handleInput stateVar)
        (updateState stateVar)

-- === NETWORK THREAD ===
networkLoop :: Handle -> MVar ClientState -> IO ()
networkLoop h stateVar = do
    serverMsg <- hGetLine h
    mePlayer <- fmap me (readMVar stateVar)
    mNewGame <- modifyMVar stateVar $ \currentState -> do
        case serverMsg of
            "REMATCH? Y/N" -> do
                let newState = currentState { statusMessage = "Rematch? (Y/N)" }
                return (newState, Nothing)
            "QUIT" -> do
                let newState = currentState { statusMessage = "Opponent has left." }
                return (newState, Nothing)
            _ | "MSG: " `isPrefixOf` serverMsg -> do
                let msg = drop 5 serverMsg
                if msg == "Invalid move." || "Welcome" `isPrefixOf` msg || msg == "Not your turn!" || msg == "Opponent disconnected."
                then do
                    let newStatus = if msg == "Invalid move." then "Invalid move!" else msg
                    let newState = currentState { statusMessage = newStatus }
                    return (newState, Nothing)
                else do
                    let opponentMsg = "Opponent: " ++ msg
                    let newHistory = take 6 (opponentMsg : chatHistory currentState)
                    let newState = currentState { chatHistory = newHistory }
                    return (newState, Nothing)
            _ -> do
                let newGame = read serverMsg :: Game
                let newHistory = if board newGame == board initialGame
                                 then []
                                 else chatHistory currentState
                let newMsg = case gameStatus newGame of
                                Running -> if currentPlayer newGame == mePlayer
                                           then "Your turn!"
                                           else "Waiting for opponent..."
                                Win p -> "Player " ++ show p ++ " wins!"
                                Draw  -> "It's a draw!"
                let newState = currentState { gameState = newGame
                                            , statusMessage = newMsg
                                            , chatHistory = newHistory
                                            }
                return (newState, Just newGame)
    case mNewGame of
        Just game -> case gameStatus game of
                        Win _   -> hPutStrLn h "GAME_OVER_ACK"
                        Draw    -> hPutStrLn h "GAME_OVER_ACK"
                        Running -> return ()
        Nothing -> return ()
    networkLoop h stateVar

-- === GLOSS STATE ===
updateState :: MVar ClientState -> Float -> ClientState -> IO ClientState
updateState stateVar dt _ = do
    st <- readMVar stateVar
    let st' = applyRepeats dt st
    _ <- swapMVar stateVar st'
    return st'

drawState :: ClientState -> IO Picture
drawState state = return $ Pictures
    [ drawBoard (gameState state)
    , drawMessage (statusMessage state)
    , drawRematchButtons state
    , drawChatHistory (chatHistory state)
    , drawChatInput (chatInput state) (chatCursor state) (selectAll state)
    , drawWinningLine (gameState state)
    ]

-- === INPUT HANDLERS ===
handleInput :: MVar ClientState -> Event -> ClientState -> IO ClientState

-- CASE 1: MOUSE CLICK
handleInput stateVar (EventKey (MouseButton LeftButton) Down _ (mx, my)) state
    | statusMessage state == "Rematch? (Y/N)" && isClickInRect (mx, my) yesButton = do
        hPutStrLn (serverHandle state) "Y"
        let newState = state { statusMessage = "Waiting for opponent..." }
        _ <- swapMVar stateVar newState
        return newState
    | statusMessage state == "Rematch? (Y/N)" && isClickInRect (mx, my) noButton = do
        hPutStrLn (serverHandle state) "N"
        let newState = state { statusMessage = "Quitting." }
        _ <- swapMVar stateVar newState
        return newState
    | currentPlayer (gameState state) == me state && gameStatus (gameState state) == Running = do
        case pixelToGrid (mx, my) of
            Just (r, c) -> do
                let moveStr = show (r + 1) ++ " " ++ show (c + 1) -- server 1-based
                hPutStrLn (serverHandle state) moveStr
            Nothing -> return ()
        return state
    | otherwise = return state

-- CASE 2: ENTER to send chat
handleInput stateVar (EventKey (SpecialKey KeyEnter) Down _ _) state = do
    let chatMsg = chatInput state
    unless (null chatMsg) $ hPutStrLn (serverHandle state) ("CHAT:" ++ chatMsg)
    let newHistory = if null chatMsg
                     then chatHistory state
                     else take 6 (("You: " ++ chatMsg) : chatHistory state)
    let newState = state { chatInput = "", chatCursor = 0, chatHistory = newHistory }
    _ <- swapMVar stateVar newState
    return newState

-- CASE 3: BACKSPACE
handleInput stateVar (EventKey (SpecialKey KeyBackspace) Down (Modifiers _ ctrl _) _) state
  | statusMessage state == "Rematch? (Y/N)" = return state
  | selectAll state = do
      let newState = state { chatInput = "", chatCursor = 0, selectAll = False }
      _ <- swapMVar stateVar newState
      return newState
  | ctrl == Down = do
      let (s', i') = deletePrevWordAt (chatInput state) (chatCursor state)
      let newState = state { chatInput = s', chatCursor = i' }
      _ <- swapMVar stateVar newState
      return newState
  | otherwise = do
      let (s', i') = backspaceAt (chatInput state) (chatCursor state)
      let newState = state { chatInput = s', chatCursor = i' }
      _ <- swapMVar stateVar newState
      return newState

-- CASE 3a: Fallback BACKSPACE as Char '\b' (some layouts send this)
handleInput stateVar (EventKey (Char '\b') Down (Modifiers _ ctrl _) _) state
  | statusMessage state == "Rematch? (Y/N)" = return state
  | ctrl == Down = do
      let (s', i') = deletePrevWordAt (chatInput state) (chatCursor state)
      let newState = state { chatInput = s', chatCursor = i' }
      _ <- swapMVar stateVar newState
      return newState
  | otherwise = do
      let (s', i') = backspaceAt (chatInput state) (chatCursor state)
      let newState = state { chatInput = s', chatCursor = i' }
      _ <- swapMVar stateVar newState
      return newState

-- CASE 3b: DELETE (forward delete)
handleInput stateVar (EventKey (SpecialKey KeyDelete) Down (Modifiers _ ctrl _) _) state
  | statusMessage state == "Rematch? (Y/N)" = return state
  | selectAll state = do
      let newState = state { chatInput = "", chatCursor = 0, selectAll = False }
      _ <- swapMVar stateVar newState
      return newState
  | ctrl == Down = do
      let s  = chatInput state
          i  = chatCursor state
          right = drop i s
          right' = dropWhile isSpace right
          right''= dropWhile (not . isSpace) right'
          s' = take i s ++ right''
      let newState = state { chatInput = s', chatCursor = i }
      _ <- swapMVar stateVar newState
      return newState
  | otherwise = do
      let (s', i') = deleteAt (chatInput state) (chatCursor state)
      let newState = state { chatInput = s', chatCursor = i' }
      _ <- swapMVar stateVar newState
      return newState

-- CASE 3c: Fallback BACKSPACE as Char '\DEL' (một số hệ gửi 127 thay vì KeyBackspace)
handleInput stateVar (EventKey (Char '\DEL') Down (Modifiers _ ctrl _) _) state
  | statusMessage state == "Rematch? (Y/N)" = return state
  | ctrl == Down = do
      let (s', i') = deletePrevWordAt (chatInput state) (chatCursor state)
      let newState = state { chatInput = s', chatCursor = i' }
      _ <- swapMVar stateVar newState
      return newState
  | otherwise = do
      let (s', i') = backspaceAt (chatInput state) (chatCursor state)
      let newState = state { chatInput = s', chatCursor = i' }
      _ <- swapMVar stateVar newState
      return newState


-- CASE 4a: CTRL+A select all
handleInput stateVar (EventKey (Char 'a') Down (Modifiers _ Down _) _) state = do
    let s = chatInput state
    let newState = state { chatCursor = length s, selectAll = not (null s) }
    _ <- swapMVar stateVar newState
    return newState


-- CASE 3d: BACKSPACE Key Up
handleInput stateVar (EventKey (SpecialKey KeyBackspace) Up _ _) state = do
    let newState = state { backHold = False }
    _ <- swapMVar stateVar newState
    return newState

-- CASE 3e: BACKSPACE Char '\b' Up
handleInput stateVar (EventKey (Char '\b') Up _ _) state = do
    let newState = state { backHold = False }
    _ <- swapMVar stateVar newState
    return newState

-- CASE 3f: BACKSPACE Char '\DEL' Up
handleInput stateVar (EventKey (Char '\DEL') Up _ _) state = do
    let newState = state { backHold = False }
    _ <- swapMVar stateVar newState
    return newState

-- CASE 4: SPACE
handleInput stateVar (EventKey (SpecialKey KeySpace) Down _ _) state
  | statusMessage state == "Rematch? (Y/N)" = return state
  | otherwise = do
      let baseS = if selectAll state then " " else chatInput state
          baseI = if selectAll state then 0 else chatCursor state
          (s', i') = insertCharAt ' ' baseS baseI
      let newState = state { chatInput = s', chatCursor = i', selectAll = False }
      _ <- swapMVar stateVar newState
      return newState

-- CASE 5: TYPING
handleInput stateVar (EventKey (Char c) Down _ _) state
    | statusMessage state == "Rematch? (Y/N)" = do
        let decision = toUpper c
        if decision == 'Y'
        then do
            hPutStrLn (serverHandle state) "Y"
            let newState = state { statusMessage = "Waiting for opponent..." }
            _ <- swapMVar stateVar newState
            return newState
        else if decision == 'N'
        then do
            hPutStrLn (serverHandle state) "N"
            let newState = state { statusMessage = "Quitting." }
            _ <- swapMVar stateVar newState
            return newState
        else
            return state
    | otherwise =
        if c >= ' ' then do
            let (s', i') = insertCharAt c (chatInput state) (chatCursor state)
            let newState = state { chatInput = s', chatCursor = i', selectAll = False }
            _ <- swapMVar stateVar newState
            return newState
        else return state

-- CASE 6: LEFT/RIGHT/HOME/END + Ctrl-word jumps
handleInput stateVar (EventKey (SpecialKey KeyLeft) Down (Modifiers _ ctrl _) _) state = do
    let s = chatInput state; i = chatCursor state
    let i' = if ctrl == Down then moveWordLeft s i else max 0 (i - 1)
    let newState = state { chatCursor = i', selectAll = False, leftHold = True, leftCtrlHold = (ctrl == Down), leftTimer = 0, leftInitial = True }
    _ <- swapMVar stateVar newState
    return newState

handleInput stateVar (EventKey (SpecialKey KeyRight) Down (Modifiers _ ctrl _) _) state = do
    let s = chatInput state; i = chatCursor state
    let i' = if ctrl == Down then moveWordRight s i else min (length s) (i + 1)
    let newState = state { chatCursor = i', selectAll = False
                         , rightHold = True, rightCtrlHold = (ctrl == Down)
                         , rightTimer = 0, rightInitial = True }
    _ <- swapMVar stateVar newState
    return newState

handleInput stateVar (EventKey (SpecialKey KeyHome) Down _ _) state = do
    let newState = state { chatCursor = 0, leftHold = False, rightHold = False, backHold = False }
    _ <- swapMVar stateVar newState
    return newState

handleInput stateVar (EventKey (SpecialKey KeyEnd) Down _ _) state = do
    let newState = state { chatCursor = length (chatInput state), leftHold = False, rightHold = False, backHold = False }
    _ <- swapMVar stateVar newState
    return newState


-- CASE 6a: KEY UP Left/Right
handleInput stateVar (EventKey (SpecialKey KeyLeft) Up _ _) state = do
    let newState = state { leftHold = False }
    _ <- swapMVar stateVar newState
    return newState

handleInput stateVar (EventKey (SpecialKey KeyRight) Up _ _) state = do
    let newState = state { rightHold = False }
    _ <- swapMVar stateVar newState
    return newState

-- CASE 7: OTHER
handleInput _ _ state = return state

-- === DRAWING HELPERS ===
drawMessage :: String -> Picture
drawMessage msg =
    let msgColor = case msg of
                     "Invalid move!" -> red
                     _ | "Waiting" `isPrefixOf` msg -> green
                     _ | "Rematch" `isPrefixOf` msg -> green
                     _ -> white
    in translate 0 (-200) $ scale 0.2 0.2 $ color msgColor $ Text msg

drawChatHistory :: [String] -> Picture
drawChatHistory history =
    translate (fromIntegral (- (windowWidth `div` 2) + 20))
              (fromIntegral (- (windowHeight `div` 2) + 180)) $
    Pictures $ zipWith drawLine [0..] (reverse history)
  where
    drawLine :: Int -> String -> Picture
    drawLine i s = translate 0 (fromIntegral (-i) * 25) $ scale 0.15 0.15 $
                   if "You:" `isPrefixOf` s
                   then color cyan (Text s)
                   else color white (Text s)

drawChatInput :: String -> Int -> Bool -> Picture
drawChatInput input cursorPos selAll =
    let (l,r) = splitAt cursorPos input
        shown = "> " ++ l ++ "|" ++ r
    in let baseX = fromIntegral (- (windowWidth `div` 2) + 20)
           baseY = fromIntegral (- (windowHeight `div` 2) + 40)
           bg = if selAll && not (null input)
                then translate (baseX + 200) (baseY + 3) $ color (makeColor 0.3 0.3 0.8 0.5) $ rectangleSolid 400 28
                else Blank
       in Pictures [ bg
                   , translate baseX baseY $ scale 0.2 0.2 $ color cyan $ Text shown
                   ]

drawRematchButtons :: ClientState -> Picture
drawRematchButtons state
    | statusMessage state == "Rematch? (Y/N)" = Pictures [yesPic, noPic]
    | otherwise = Blank
  where
    (x1, y1, w1, h1) = yesButton
    (x2, y2, w2, h2) = noButton
    yesPic = Pictures [ translate x1 y1 $ color green $ rectangleSolid w1 h1
                      , translate (x1 - w1/2 + 10) (y1 - 10) $ scale 0.2 0.2 $ color black $ Text "YES"]
    noPic  = Pictures [ translate x2 y2 $ color red   $ rectangleSolid w2 h2
                      , translate (x2 - w2/2 + 10) (y2 - 10) $ scale 0.2 0.2 $ color black $ Text "NO"]

drawBoard :: Game -> Picture
drawBoard g =
    translate 0 80 $ Pictures (gridLines ++ pieces)
  where
    b = board g
    boardPixelWidth = fromIntegral (boardSize * cellSize)
    halfWidth = boardPixelWidth / 2
    gridLines =
        [ Color white (Line [(x, halfWidth), (x, -halfWidth)])
          | c <- [0..boardSize], let x = fromIntegral (c * cellSize) - halfWidth ] ++
        [ Color white (Line [(-halfWidth, y), (halfWidth, y)])
          | r <- [0..boardSize], let y = fromIntegral (r * cellSize) - halfWidth ]
    pieces =
        [ translate (x + fromIntegral cellSize / 2 - halfWidth)
                    (halfWidth - y - fromIntegral cellSize / 2)
                    (drawCell (b !! r !! c))
        | r <- [0..boardSize-1], c <- [0..boardSize-1]
        , let x = fromIntegral (c * cellSize)
        , let y = fromIntegral (r * cellSize) ]

drawCell :: Cell -> Picture
drawCell Empty     = Blank
drawCell (Taken X) = drawX
drawCell (Taken O) = drawO

drawO :: Picture
drawO = Color blue $ ThickCircle (fromIntegral cellSize * 0.4) 5

drawX :: Picture
drawX = Color red $ Pictures
    [ Rotate 45 (rectangle len thickness)
    , Rotate (-45) (rectangle len thickness)
    ]
  where
    len = fromIntegral cellSize * 0.45
    thickness = 8.0
    rectangle :: Float -> Float -> Picture
    rectangle l w = Polygon [(-l,- (w / 2)), (l,- (w / 2)), (l, w/2), (-l, w/2)]

-- === WIN LINE ===
-- Chuyển (row,col) 0-based sang pixel tâm ô
gridToPixel :: (Int, Int) -> (Float, Float)
gridToPixel (r, c) =
    let boardPixelWidth = fromIntegral (boardSize * cellSize)
        halfWidth = boardPixelWidth / 2
        x_cell = fromIntegral (c * cellSize)
        y_cell = fromIntegral (r * cellSize)
        x_pos = x_cell + fromIntegral cellSize / 2 - halfWidth
        y_pos = halfWidth - y_cell - fromIntegral cellSize / 2
    in (x_pos, y_pos)

-- Chuẩn hóa chỉ số 1-based từ server về 0-based nếu cần
normalizeWinCells :: [(Int,Int)] -> [(Int,Int)]
normalizeWinCells cells =
  let rs = map fst cells
      cs = map snd cells
      rmax = if null rs then 0 else maximum rs
      cmax = if null cs then 0 else maximum cs
  in if rmax >= boardSize || cmax >= boardSize
        then map (\(r,c) -> (r-1, c-1)) cells
        else cells

-- Chọn hai điểm xa nhau nhất theo pixel
farthestPairPixels :: [(Int,Int)] -> ((Float,Float),(Float,Float))
farthestPairPixels cells =
  let pts = map gridToPixel cells
      pairs = [ (a,b) | (i,a) <- zip [0..] pts, (j,b) <- zip [0..] pts, j > i ]
      dist2 ((x1,y1),(x2,y2)) = let dx = x2 - x1; dy = y2 - y1 in dx*dx + dy*dy
  in case pairs of
       []      -> case pts of { (p:_) -> (p,p); _ -> ((0,0),(0,0)) }
       _       -> maximumBy (comparing dist2) pairs


-- Chọn 2 đầu mút theo bất biến: ngang, dọc, chéo r-c hằng, chéo r+c hằng.
-- Trả về Nothing nếu không khớp pattern.
endpointsFromInvariants :: [(Int,Int)] -> Maybe ((Int,Int),(Int,Int))
endpointsFromInvariants cells
  | null cells = Nothing
  | all (== r0) rs = Just ((r0, minimum cs), (r0, maximum cs))
  | all (== c0) cs = Just ((minimum rs, c0), (maximum rs, c0))
  | all (== d0) ds = Just ((minimum rs, minimum cs), (maximum rs, maximum cs)) -- r-c hằng (\)
  | all (== s0) ss = Just ((minimum rs, maximum cs), (maximum rs, minimum cs)) -- r+c hằng (/)
  | otherwise      = Nothing
  where
    rs = map fst cells
    cs = map snd cells
    r0 = head rs
    c0 = head cs
    ds = zipWith (\r c -> r - c) rs cs
    ss = zipWith (\r c -> r + c) rs cs
    d0 = head ds
    s0 = head ss

-- Thử cả (r,c) và (c,r), nếu cả hai đều fail thì rơi về farthestPairPixels
chooseEndpoints :: [(Int,Int)] -> ((Float,Float),(Float,Float))
chooseEndpoints cells =
  case endpointsFromInvariants cells of
    Just ((r1,c1),(r2,c2)) -> (gridToPixel (r1,c1), gridToPixel (r2,c2))
    Nothing ->
      let swapped = map (\(r,c) -> (c,r)) cells
      in case endpointsFromInvariants swapped of
           Just ((r1,c1),(r2,c2)) -> (gridToPixel (c1,r1), gridToPixel (c2,r2))
           Nothing -> farthestPairPixels cells

drawWinningLine :: Game -> Picture
drawWinningLine g =
  case gameStatus g of
    Win player ->
      let cells0 = winningCells g
          cells  = normalizeWinCells cells0
      in if length cells >= winCondition
         then let ((x1,y1),(x2,y2)) = farthestPairPixels cells
                  lineColor = if player == X then red else blue
              in translate 0 80 $ color lineColor $ drawThickLine 5.0 (x1,y1) (x2,y2)
         else Blank
    _ -> Blank

-- === UTILITY ===
isClickInRect :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
isClickInRect (clickX, clickY) (rectX, rectY, rectW, rectH) =
    clickX >= rectX - rectW / 2 &&
    clickX <= rectX + rectW / 2 &&
    clickY >= rectY - rectH / 2 &&
    clickY <= rectY + rectH / 2

-- Convert mouse to (row,col) 0-based. Board translated +80 in Y.
pixelToGrid :: (Float, Float) -> Maybe (Int, Int)
pixelToGrid (mouseX, mouseY) =
    let boardPixelWidth = fromIntegral (boardSize * cellSize)
        halfWidth = boardPixelWidth / 2
        boardTopLeftY = 80 + halfWidth
        boardTopLeftX = -halfWidth
        clickX = mouseX - boardTopLeftX
        clickY = boardTopLeftY - mouseY
    in if clickX < 0 || clickX > boardPixelWidth || clickY < 0 || clickY > boardPixelWidth
       then Nothing
       else
         let r = floor $ clickY / fromIntegral cellSize
             c = floor $ clickX / fromIntegral cellSize
         in if r >= 0 && r < boardSize && c >= 0 && c < boardSize
            then Just (r, c)
            else Nothing

-- Render thick line between two points by rotating a rectangle
drawThickLine :: Float -> (Float, Float) -> (Float, Float) -> Picture
drawThickLine thickness (x1, y1) (x2, y2) =
    let dx = x2 - x1
        dy = y2 - y1
        len = sqrt (dx * dx + dy * dy)
        angle = atan2 dy dx * 180 / pi
        mx = (x1 + x2) / 2
        my = (y1 + y2) / 2
    in translate mx my $ Rotate angle $ rectangleSolid len thickness

-- === TEXT EDIT HELPERS ===
insertCharAt :: Char -> String -> Int -> (String, Int)
insertCharAt c s i = let (l,r) = splitAt i s in (l ++ [c] ++ r, i+1)

backspaceAt :: String -> Int -> (String, Int)
backspaceAt s i
  | i <= 0    = (s, 0)
  | otherwise = let (l,r) = splitAt i s in (init l ++ r, i-1)

deleteAt :: String -> Int -> (String, Int)
deleteAt s i
  | i >= length s = (s, i)
  | otherwise     = let (l,r) = splitAt i s in (l ++ drop 1 r, i)

deletePrevWordAt :: String -> Int -> (String, Int)
deletePrevWordAt s i =
  let (l,r) = splitAt i s
      dropSpaces = reverse . dropWhile isSpace . reverse
      dropWord t = reverse . dropWhile (not . isSpace) . reverse $ t
      l' = dropSpaces (dropWord (dropSpaces l))
      i' = length l'
  in (l' ++ r, i')

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

-- === REPEAT KEYS ===
initialDelay :: Float
initialDelay = 0.35
repeatRate :: Float
repeatRate = 0.05

applyRepeats :: Float -> ClientState -> ClientState
applyRepeats dt st =
  let -- Left
      (st1, _) =
        if leftHold st
          then let t = leftTimer st + dt
                   initP = leftInitial st
               in if (initP && t >= initialDelay) || (not initP && t >= repeatRate)
                    then let s = chatInput st; i = chatCursor st
                             i' = if leftCtrlHold st then moveWordLeft s i else max 0 (i - 1)
                             st' = st { chatCursor = i', selectAll = False, leftTimer = 0, leftInitial = False }
                         in (st', True)
                    else (st { leftTimer = t }, False)
          else (st { leftTimer = 0, leftInitial = True }, False)
      -- Right
      (st2, _) =
        if rightHold st1
          then let t = rightTimer st1 + dt
                   initP = rightInitial st1
               in if (initP && t >= initialDelay) || (not initP && t >= repeatRate)
                    then let s = chatInput st1; i = chatCursor st1; l = length s
                             i' = if rightCtrlHold st1 then moveWordRight s i else min l (i + 1)
                             st' = st1 { chatCursor = i', selectAll = False, rightTimer = 0, rightInitial = False }
                         in (st', True)
                    else (st1 { rightTimer = t }, False)
          else (st1 { rightTimer = 0, rightInitial = True }, False)
      -- Backspace
      st3 =
        if backHold st2
          then let t = backTimer st2 + dt
                   initP = backInitial st2
               in if (initP && t >= initialDelay) || (not initP && t >= repeatRate)
                    then let s = chatInput st2; i = chatCursor st2
                             (s', i') = if backCtrlHold st2 then deletePrevWordAt s i else backspaceAt s i
                         in st2 { chatInput = s', chatCursor = i', selectAll = False, backTimer = 0, backInitial = False }
                    else st2 { backTimer = t }
          else st2 { backTimer = 0, backInitial = True }
  in st3
