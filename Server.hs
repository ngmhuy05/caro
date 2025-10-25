{-# LANGUAGE OverloadedStrings #-}
import Network.Socket
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import System.IO (hSetEncoding, utf8, Handle, hSetBuffering, BufferMode(LineBuffering), hPutStrLn, hGetLine, IOMode(ReadWriteMode), stdout)
import GameState
import Data.List (transpose)
import Control.Exception (catch, SomeException)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4444 (tupleToHostAddress (0,0,0,0)))
    listen sock 2
    putStrLn "Server dang lang nghe tai port 4444..."
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    (conn1, _) <- accept sock
    h1 <- socketToHandle conn1 ReadWriteMode
    hSetBuffering h1 LineBuffering
    hSetEncoding h1 utf8
    hPutStrLn h1 "PLAYER: X"
    hPutStrLn h1 "MSG: Chao mung nguoi choi 1 (X). Dang cho nguoi choi 2..."

    (conn2, _) <- accept sock
    h2 <- socketToHandle conn2 ReadWriteMode
    hSetBuffering h2 LineBuffering
    hSetEncoding h2 utf8
    hPutStrLn h2 "PLAYER: O"
    hPutStrLn h2 "MSG: Chao mung nguoi choi 2 (O)."

    putStrLn "Da ghep cap! Bat dau tran dau moi."
    
    gameStateVar <- newMVar initialGame
    rematchVarX <- newEmptyMVar :: IO (MVar Bool)
    rematchVarO <- newEmptyMVar :: IO (MVar Bool)

    sendGame h1 gameStateVar
    sendGame h2 gameStateVar

    forkIO (playerLoop h1 h2 gameStateVar X rematchVarX rematchVarO)
    forkIO (playerLoop h2 h1 gameStateVar O rematchVarO rematchVarX)
    
    mainLoop sock

playerLoop :: Handle -> Handle -> MVar Game -> Player -> MVar Bool -> MVar Bool -> IO ()
playerLoop hMy hOther gameStateVar me myRematchVar otherRematchVar = (gameSession `catch` handler)
  where
    gameSession = do
        gameEnded <- gameLoop
        if gameEnded
        then do
            hPutStrLn hMy "REMATCH? Y/N"
            decisionLine <- hGetLine hMy
            let myDecision = (decisionLine == "Y")
            
            putMVar myRematchVar myDecision
            otherDecision <- readMVar otherRematchVar

            if myDecision && otherDecision
            then do
                putStrLn $ "Player " ++ show me ++ " initiating rematch."
                if me == X
                then do
                    modifyMVar_ gameStateVar (\_ -> return initialGame)
                    sendGame hMy gameStateVar
                    sendGame hOther gameStateVar
                else
                    return ()
                playerLoop hMy hOther gameStateVar me myRematchVar otherRematchVar
            else do
                hPutStrLn hMy "QUIT"
                putStrLn $ "Player " ++ show me ++ " quitting session."
        else
            return ()

    gameLoop :: IO Bool
    gameLoop = do
        line <- hGetLine hMy
        
        if line == "GAME_OVER_ACK"
        then return True
        else do
            (gameEnded, _) <- modifyMVar gameStateVar $ \g -> do
                if gameStatus g /= Running then return (g, (True, g))
                else if currentPlayer g /= me then do
                    hPutStrLn hMy "MSG: Khong phai luot cua ban!"
                    return (g, (False, g))
                else do
                    let (finalGame, valid) = applyMove g me line
                    if not valid then do
                        hPutStrLn hMy "MSG: Nuoc di khong hop le."
                        hPutStrLn hMy (show finalGame)
                        return (finalGame, (False, finalGame))
                    else do
                        hPutStrLn hMy (show finalGame)
                        hPutStrLn hOther (show finalGame)
                        let ended = gameStatus finalGame /= Running
                        return (finalGame, (ended, finalGame))

            if gameEnded
            then return True
            else gameLoop

    handler :: SomeException -> IO ()
    handler e = do
        putStrLn $ "Player " ++ show me ++ " disconnected. Terminating loop."
        putMVar myRematchVar False
        return ()

sendGame :: Handle -> MVar Game -> IO ()
sendGame h gameVar = do
    g <- readMVar gameVar
    hPutStrLn h (show g)

applyMove :: Game -> Player -> String -> (Game, Bool)
applyMove g me line =
    case words line of
        [rStr, cStr] ->
            case (reads rStr, reads cStr) of
                ([(r, "")], [(c, "")]) ->
                    let r_idx = r - 1
                        c_idx = c - 1
                    in if validMove (board g) r_idx c_idx
                       then (updateGameStatus (makeMove g me r_idx c_idx) me r_idx c_idx, True)
                       else (g, False)
                _ -> (g, False)
        _ -> (g, False)

validMove :: Board -> Int -> Int -> Bool
validMove b r c = r >= 0 && r < boardSize && c >= 0 && c < boardSize && (b !! r !! c == Empty)

makeMove :: Game -> Player -> Int -> Int -> Game
makeMove g p r c =
    let newBoard = take r (board g)
                    ++ [take c (board g !! r)
                    ++ [Taken p]
                    ++ drop (c + 1) (board g !! r)]
                    ++ drop (r + 1) (board g)
    in g { board = newBoard, currentPlayer = nextPlayer p }

updateGameStatus :: Game -> Player -> Int -> Int -> Game
updateGameStatus g lastPlayer r c =
    if checkWin (board g) lastPlayer r c
    then g { gameStatus = Win lastPlayer }
    else if checkDraw (board g)
    then g { gameStatus = Draw }
    else g

checkWin :: Board -> Player -> Int -> Int -> Bool
checkWin b p r c =
    any (>= winCondition) [
        1 + countDir b p r c 1 0  + countDir b p r c (-1) 0,
        1 + countDir b p r c 0 1  + countDir b p r c 0 (-1),
        1 + countDir b p r c 1 1  + countDir b p r c (-1) (-1),
        1 + countDir b p r c 1 (-1) + countDir b p r c (-1) 1
    ]

countDir :: Board -> Player -> Int -> Int -> Int -> Int -> Int
countDir b p r c dr dc =
    let r' = r + dr
        c' = c + dc
    in if r' < 0 || r' >= boardSize || c' < 0 || c' >= boardSize || (b !! r' !! c' /= Taken p)
       then 0
       else 1 + countDir b p r' c' dr dc

checkDraw :: Board -> Bool
checkDraw b = all (all (/= Empty)) b