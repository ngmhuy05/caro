-- File: GUI.hs
module GUI (
    runGUI
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent (forkIO, MVar, newMVar, modifyMVar, readMVar, swapMVar)
import Control.Monad (when, unless)
import System.IO (Handle, hPutStrLn, hGetLine)
import Data.Char (toUpper)
import Data.List (isPrefixOf)

import GameState (Player(..), Game(..), GameStatus(..), Cell(..), Board, boardSize, initialGame, winCondition)

-- Window dimensions and constants
windowWidth, windowHeight, cellSize :: Int
padding :: Float
windowWidth = 600
windowHeight = 800 
cellSize = 50
padding = (fromIntegral windowWidth - fromIntegral boardSize * fromIntegral cellSize) / 2

-- Dinh nghia nut Rematch
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
    chatHistory   :: [String]
}

-- === MAIN GUI FUNCTION ===
runGUI :: Handle -> Player -> IO ()
runGUI h me = do
    let initialState = ClientState initialGame me "Waiting for server..." h "" []
    stateVar <- newMVar initialState

    forkIO (networkLoop h stateVar)

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

            -- (Logic nay da dung)
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

            -- Game state update
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

                let newState = currentState { gameState = newGame, statusMessage = newMsg, chatHistory = newHistory }
                return (newState, Just newGame)


    -- Handle IO (sending ACK) outside the MVar
    case mNewGame of
        Just game ->
            case gameStatus game of
                Win _   -> hPutStrLn h "GAME_OVER_ACK"
                Draw    -> hPutStrLn h "GAME_OVER_ACK"
                Running -> return ()
        Nothing -> return ()

    networkLoop h stateVar -- Loop

-- === GLOSS FUNCTIONS ===

-- Update: Sync Gloss state with MVar
updateState :: MVar ClientState -> Float -> ClientState -> IO ClientState
updateState stateVar _ _ = readMVar stateVar

-- Draw: Render the current state
drawState :: ClientState -> IO Picture
drawState state = return $ Pictures [
        drawBoard (gameState state),
        drawMessage (statusMessage state),
        drawRematchButtons state,
        drawChatHistory (chatHistory state),
        drawChatInput (chatInput state),
        drawWinningLine (gameState state)
    ]

-- Input: Handle mouse clicks and key presses
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
                let moveStr = show (r + 1) ++ " " ++ show (c + 1)
                hPutStrLn (serverHandle state) moveStr
            Nothing -> return ()
        return state

    | otherwise = return state

-- CASE 2: KEY PRESS (ENTER - GUI CHAT)
handleInput stateVar (EventKey (SpecialKey KeyEnter) Down _ _) state = do
    let chatMsg = chatInput state

    unless (null chatMsg) $ hPutStrLn (serverHandle state) ("CHAT:" ++ chatMsg)

    let newHistory = if null chatMsg
                     then chatHistory state
                     else take 6 (("You: " ++ chatMsg) : chatHistory state)

    let newState = state { chatInput = "", chatHistory = newHistory }
    _ <- swapMVar stateVar newState
    return newState

-- CASE 3: KEY PRESS (BACKSPACE - XOA CHAT)
handleInput stateVar (EventKey (SpecialKey KeyBackspace) Down _ _) state = do
    let newChat = if null (chatInput state) then "" else init (chatInput state)
    let newState = state { chatInput = newChat }
    _ <- swapMVar stateVar newState
    return newState

-- CASE 4: KEY PRESS (KY TU THUONG - GO CHAT)
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
            -- *** SUA LOI TYPO: "swapMVAr" thanh "swapMVar" ***
            _ <- swapMVar stateVar newState 
            return newState
        else
            return state

    | otherwise = do
        let newChat = chatInput state ++ [c]
        let newState = state { chatInput = newChat }
        _ <- swapMVar stateVar newState
        return newState

-- CASE 5: OTHER EVENTS
handleInput _ _ state = return state


-- === DRAWING HELPER FUNCTIONS ===

-- Ve thong bao (Vi tri da dung)
drawMessage :: String -> Picture
drawMessage msg =
    let msgColor = case msg of
                     "Invalid move!" -> red
                     _ | "Waiting" `isPrefixOf` msg -> green
                     _ | "Rematch" `isPrefixOf` msg -> green
                     _ -> white -- "Your turn!", "Player X wins!", etc.
    in
    translate 0 (-200) $ 
    scale 0.2 0.2 $
    color msgColor $
    Text msg

-- Ve lich su chat (Vi tri da dung)
drawChatHistory :: [String] -> Picture
drawChatHistory history =
    translate (fromIntegral (- (windowWidth `div` 2) + 20)) (fromIntegral (- (windowHeight `div` 2) + 180)) $
    Pictures $ zipWith drawLine [0..] (reverse history)
  where
    drawLine :: Int -> String -> Picture
    drawLine i s = translate 0 (fromIntegral (-i) * 25) $ scale 0.15 0.15 $
                 if "You:" `isPrefixOf` s
                 then color cyan (Text s)
                 else color white (Text s) 

-- Ve o nhap lieu chat (Vi tri da dung)
drawChatInput :: String -> Picture
drawChatInput input =
    translate (fromIntegral (- (windowWidth `div` 2) + 20)) (fromIntegral (- (windowHeight `div` 2) + 40)) $
    scale 0.2 0.2 $
    color cyan $
    Text ("> " ++ input ++ "_") 

-- Ve nut Rematch
drawRematchButtons :: ClientState -> Picture
drawRematchButtons state
    | statusMessage state == "Rematch? (Y/N)" = Pictures [yesPic, noPic]
    | otherwise = Blank
  where
    (x1, y1, w1, h1) = yesButton
    (x2, y2, w2, h2) = noButton
    yesPic = Pictures [
        translate x1 y1 $ color green $ rectangleSolid w1 h1,
        translate (x1 - w1/2 + 10) (y1 - 10) $ scale 0.2 0.2 $ color black $ Text "YES"
        ]
    noPic = Pictures [
        translate x2 y2 $ color red $ rectangleSolid w2 h2,
        translate (x2 - w2/2 + 10) (y2 - 10) $ scale 0.2 0.2 $ color black $ Text "NO"
        ]

-- Ve ban co (Vi tri da dung)
drawBoard :: Game -> Picture
drawBoard g =
    translate 0 80 $
    Pictures (gridLines ++ pieces)
  where
    b = board g
    boardPixelWidth = fromIntegral (boardSize * cellSize)
    halfWidth = boardPixelWidth / 2

    gridLines =
        [ Color white (Line [(x, halfWidth), (x, -halfWidth)])
          | c <- [0..boardSize], let x = fromIntegral (c * cellSize) - halfWidth ] ++
        [ Color white (Line [(-halfWidth, y), (halfWidth, y)])
          | r <- [00..boardSize], let y = fromIntegral (r * cellSize) - halfWidth ]

    pieces = [ translate (x + fromIntegral cellSize / 2 - halfWidth)
                         (halfWidth - y - fromIntegral cellSize / 2)
                         (drawCell (b !! r !! c))
             | r <- [0..boardSize-1], c <- [0..boardSize-1],
               let x = fromIntegral (c * cellSize),
               let y = fromIntegral (r * cellSize)
             ]

-- Ve 1 o co
drawCell :: Cell -> Picture
drawCell Empty = Blank
drawCell (Taken X) = drawX
drawCell (Taken O) = drawO

-- Ham ve O (Hinh tron)
drawO :: Picture
drawO = Color blue $ ThickCircle (fromIntegral cellSize * 0.4) 5

-- Ham ve X (Dung 2 hinh chu nhat xoay)
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

-- Ham ve duong gach thang
drawWinningLine :: Game -> Picture
drawWinningLine g =
    case gameStatus g of
        Win player ->
            let cells = winningCells g
                lineColor = if player == X then red else blue
            in if length cells >= winCondition
               then
                    let (r1, c1) = head cells
                        (r5, c5) = cells !! (winCondition - 1)
                        (x1, y1) = gridToPixel (r1, c1)
                        (x2, y2) = gridToPixel (r5, c5)
                    in
                       -- Giu do day la 5.0
                       translate 0 80 $ color lineColor $ drawThickLine 5.0 (x1, y1) (x2, y2)
               else Blank
        _ -> Blank


-- === UTILITY FUNCTIONS ===

-- Kiem tra click trong hinh chu nhat
isClickInRect :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
isClickInRect (clickX, clickY) (rectX, rectY, rectW, rectH) =
    clickX >= rectX - rectW / 2 &&
    clickX <= rectX + rectW / 2 &&
    clickY >= rectY - rectH / 2 &&
    clickY <= rectY + rectH / 2

-- Ham chuyen tu (hang, cot) sang (x, y) pixel
gridToPixel :: (Int, Int) -> (Float, Float)
gridToPixel (r, c) =
    let boardPixelWidth = fromIntegral (boardSize * cellSize)
        halfWidth = boardPixelWidth / 2

        x_cell = fromIntegral (c * cellSize)
        y_cell = fromIntegral (r * cellSize)

        x_pos = x_cell + fromIntegral cellSize / 2 - halfWidth
        y_pos = halfWidth - y_cell - fromIntegral cellSize / 2
    in (x_pos, y_pos)

-- Cap nhat phep tinh pixelToGrid de khop voi vi tri moi cua ban co (Y = 80)
pixelToGrid :: (Float, Float) -> Maybe (Int, Int)
pixelToGrid (mouseX, mouseY) =
    let
        boardPixelWidth = fromIntegral (boardSize * cellSize)
        halfWidth = boardPixelWidth / 2

        boardTopLeftY = 80 + halfWidth
        boardTopLeftX = -halfWidth

        clickX = mouseX - boardTopLeftX
        clickY = boardTopLeftY - mouseY

    -- *** SUA LOI TYPO TAI DAY: "c`licksX" thanh "clickX" ***
    in if clickX < 0 || clickX > boardPixelWidth || clickY < 0 || clickY > boardPixelWidth
       then Nothing
       else
        let r = floor $ clickY / fromIntegral cellSize
            c = floor $ clickX / fromIntegral cellSize
        in if r >= 0 && r < boardSize && c >= 0 && c < boardSize
           then Just (r, c)
           else Nothing

-- Draw a thick line between two points by rendering a rotated rectangle
drawThickLine :: Float -> (Float, Float) -> (Float, Float) -> Picture
drawThickLine thickness (x1, y1) (x2, y2) =
    let dx = x2 - x1
        dy = y2 - y1
        len = sqrt (dx * dx + dy * dy)
        angle = atan2 dy dx * 180 / pi
        mx = (x1 + x2) / 2
        my = (y1 + y2) / 2
    in translate mx my $ Rotate angle $ rectangleSolid len thickness