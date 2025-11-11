-- src/GUI.hs
module GUI (runGUI) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent (forkIO, MVar, newMVar, modifyMVar, readMVar, swapMVar)
import Control.Monad (unless)
import System.IO (Handle, hPutStrLn, hGetLine)
import Data.Char (toUpper, isSpace)
import Data.List (isPrefixOf)
import GameState (Player(..), Game(..), GameStatus(..), Cell(..), boardSize, initialGame)

-- ===== Layout =====
windowWidth, windowHeight, cellSize :: Int
windowWidth = 600
windowHeight = 800
cellSize = 50

yesButton, noButton :: (Float, Float, Float, Float)
yesButton = (-120, -360, 140, 56)  -- đặt dưới dòng Rematch?
noButton  = ( 120, -360, 140, 56)

-- ===== Client State =====
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
  }

-- ===== Main =====
runGUI :: Handle -> Player -> IO ()
runGUI h mePlayer = do
  let st0 = ClientState initialGame mePlayer "Waiting for server..." h "" 0 [] False
             False False False False False False 0 0 0 True True True
  var <- newMVar st0
  _ <- forkIO (networkLoop h var)
  playIO (InWindow "Gomoku" (windowWidth, windowHeight) (100,100))
         black 30 st0
         drawState
         (handleInput var)
         (updateState var)

-- ===== Network =====
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
          in return (st { gameState = g, statusMessage = newMsg
                        , chatHistory = if board g == board initialGame then [] else chatHistory st }, Just g)
  case mNew of
    Just g | gameStatus g /= Running -> hPutStrLn h "GAME_OVER_ACK"
    _ -> return ()
  networkLoop h var

-- ===== Tick =====
updateState :: MVar ClientState -> Float -> ClientState -> IO ClientState
updateState var dt _ = do
  st <- readMVar var
  let st' = applyRepeats dt st
  _ <- swapMVar var st'
  return st'

-- ===== Draw =====
drawState :: ClientState -> IO Picture
drawState st = return $ Pictures
  [ drawBoard (gameState st)
  , drawMessage (statusMessage st)
  , drawRematchButtons st
  , drawChatHistory (chatHistory st)
  , drawChatInput (chatInput st) (chatCursor st) (selectAll st)
  ]

drawMessage :: String -> Picture
drawMessage msg =
  let c = case msg of
            "Invalid move!" -> red
            _ | "Waiting" `isPrefixOf` msg -> green
            _ | "Rematch" `isPrefixOf` msg -> green
            _ -> white
  in translate 0 (-300) $ scale 0.2 0.2 $ color c $ Text msg

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

drawCell :: Cell -> Picture
drawCell Empty     = Blank
drawCell (Taken X) = drawX
drawCell (Taken O) = drawO

drawO :: Picture
drawO = Color blue $ ThickCircle (fromIntegral cellSize * 0.4) 5

drawX :: Picture
drawX = Color red $ Pictures
  [ Rotate 45  (Polygon [(-l,-t/2),(l,-t/2),(l,t/2),(-l,t/2)])
  , Rotate (-45) (Polygon [(-l,-t/2),(l,-t/2),(l,t/2),(-l,t/2)])
  ]
  where l = fromIntegral cellSize * 0.45
        t = 8.0

-- ===== Hit-test =====
isClickInRect :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
isClickInRect (cx, cy) (rx, ry, rw, rh) =
  cx >= rx - rw / 2 && cx <= rx + rw / 2 &&
  cy >= ry - rh / 2 && cy <= ry + rh / 2

pixelToGrid :: (Float, Float) -> Maybe (Int, Int)
pixelToGrid (mx, my) =
  let w = fromIntegral (boardSize * cellSize)
      half = w / 2
      topY = 80 + half
      leftX = -half
      cx = mx - leftX
      cy = topY - my
  in if cx < 0 || cx > w || cy < 0 || cy > w
       then Nothing
       else let r = floor $ cy / fromIntegral cellSize
                c = floor $ cx / fromIntegral cellSize
            in if r >= 0 && r < boardSize && c >= 0 && c < boardSize then Just (r,c) else Nothing

-- ===== Input =====
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

handleInput var (EventKey (SpecialKey KeyEnter) Down _ _) st = do
  let s = chatInput st
  unless (null s) $ hPutStrLn (serverHandle st) ("CHAT:" ++ s)
  let hist' = if null s then chatHistory st else take 6 (("You: " ++ s) : chatHistory st)
      st' = st { chatInput = "", chatCursor = 0, chatHistory = hist' }
  _ <- swapMVar var st'
  return st'

-- Backspace + variations
handleInput var (EventKey (SpecialKey KeyBackspace) Down (Modifiers _ ctrl _) _) st
  | statusMessage st == "Rematch? (Y/N)" = return st
  | selectAll st =
      let st' = st { chatInput = "", chatCursor = 0, selectAll = False, backHold = True, backCtrlHold = (ctrl==Down), backTimer = 0, backInitial = True }
      in swapMVar var st' >> return st'
  | ctrl == Down =
      let (s', i') = deletePrevWordAt (chatInput st) (chatCursor st)
          st' = st { chatInput = s', chatCursor = i', backHold = True, backCtrlHold = True, backTimer = 0, backInitial = True }
      in swapMVar var st' >> return st'
  | otherwise =
      let (s', i') = backspaceAt (chatInput st) (chatCursor st)
          st' = st { chatInput = s', chatCursor = i', backHold = True, backCtrlHold = False, backTimer = 0, backInitial = True }
      in swapMVar var st' >> return st'

handleInput var (EventKey (SpecialKey KeyBackspace) Up _ _) st =
  swapMVar var (st { backHold = False }) >> return (st { backHold = False })

-- Fallback delete/bs
handleInput var (EventKey (Char '\b') Down (Modifiers _ ctrl _) _) st =
  handleInput var (EventKey (SpecialKey KeyBackspace) Down (Modifiers Up ctrl Up) (0,0)) st
handleInput var (EventKey (Char '\b') Up _ _) st =
  handleInput var (EventKey (SpecialKey KeyBackspace) Up (Modifiers Up Up Up) (0,0)) st
handleInput var (EventKey (Char '\DEL') Down (Modifiers _ ctrl _) _) st =
  handleInput var (EventKey (SpecialKey KeyBackspace) Down (Modifiers Up ctrl Up) (0,0)) st
handleInput var (EventKey (Char '\DEL') Up _ _) st =
  handleInput var (EventKey (SpecialKey KeyBackspace) Up (Modifiers Up Up Up) (0,0)) st

-- Ctrl+A
handleInput var (EventKey (Char 'a') Down (Modifiers _ Down _) _) st =
  let s = chatInput st; st' = st { chatCursor = length s, selectAll = not (null s) }
  in swapMVar var st' >> return st'

-- Space
handleInput var (EventKey (SpecialKey KeySpace) Down _ _) st
  | statusMessage st == "Rematch? (Y/N)" = return st
  | otherwise =
      let baseS = if selectAll st then " " else chatInput st
          baseI = if selectAll st then 0   else chatCursor st
          (s', i') = insertCharAt ' ' baseS baseI
          st' = st { chatInput = s', chatCursor = i', selectAll = False }
      in swapMVar var st' >> return st'

-- Typing / Y/N in rematch
handleInput var (EventKey (Char c) Down _ _) st
  | statusMessage st == "Rematch? (Y/N)" =
      case toUpper c of
        'Y' -> hPutStrLn (serverHandle st) "Y" >> swapMVar var (st { statusMessage = "Waiting for opponent..." }) >> return (st { statusMessage = "Waiting for opponent..." })
        'N' -> hPutStrLn (serverHandle st) "N" >> swapMVar var (st { statusMessage = "Quitting." }) >> return (st { statusMessage = "Quitting." })
        _   -> return st
  | c >= ' ' =
      let (s', i') = insertCharAt c (chatInput st) (chatCursor st)
      in swapMVar var (st { chatInput = s', chatCursor = i', selectAll = False }) >> return (st { chatInput = s', chatCursor = i', selectAll = False })
  | otherwise = return st

handleInput _ _ st = return st

-- ===== Text edit helpers =====
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
initialDelay, repeatRate :: Float
initialDelay = 0.35
repeatRate   = 0.05

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
  in st3
