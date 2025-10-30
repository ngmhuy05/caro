-- File: Server.hs
{-# LANGUAGE OverloadedStrings #-}
import Network.Socket
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import System.IO (hSetEncoding, utf8, Handle, hSetBuffering, BufferMode(LineBuffering), hPutStrLn, hGetLine, hPrint, IOMode(ReadWriteMode), stdout)
import GameState
import Data.List (transpose, isPrefixOf, findIndex, find, tails) -- <<< THEM findIndex va tails
import Control.Exception (catch, SomeException)
import Control.Monad (unless, when)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetEncoding stdout utf8 

    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4444 (tupleToHostAddress (0,0,0,0)))
    listen sock 2
    putStrLn "Server online, waiting for connections..."
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    (conn1, _) <- accept sock
    h1 <- socketToHandle conn1 ReadWriteMode
    hSetBuffering h1 LineBuffering
    hSetEncoding h1 utf8
    hPutStrLn h1 "PLAYER: X"
    hPutStrLn h1 "MSG: Welcome player (X). Waiting for opponent..." -- Co dau cach

    (conn2, _) <- accept sock
    h2 <- socketToHandle conn2 ReadWriteMode
    hSetBuffering h2 LineBuffering
    hSetEncoding h2 utf8
    hPutStrLn h2 "PLAYER: O"
    hPutStrLn h2 "MSG: Welcome player (O)." -- Co dau cach

    putStrLn "Pair matched! Starting game."

    gameStateVar <- newMVar initialGame
    rematchVarX <- newEmptyMVar :: IO (MVar Bool)
    rematchVarO <- newEmptyMVar :: IO (MVar Bool)

    sendGame h1 gameStateVar
    sendGame h2 gameStateVar

    forkIO (playerLoop h1 h2 gameStateVar X rematchVarX rematchVarO)
    forkIO (playerLoop h2 h1 gameStateVar O rematchVarO rematchVarX)

    mainLoop sock

playerLoop :: Handle -> Handle -> MVar Game -> Player -> MVar Bool -> MVar Bool -> IO ()
playerLoop hMy hOther gameStateVar me myRematchVar otherRematchVar = gameSession `catch` handler
  where
    gameSession = do
        gameEnded <- gameLoop

        when gameEnded $ do
            putStrLn $ "Player " ++ show me ++ " Game ended"
            threadDelay 3000000 

            hPutStrLn hMy "REMATCH? Y/N"
            decisionLine <- hGetLine hMy
            let myDecision = decisionLine == "Y"

            putMVar myRematchVar myDecision
            otherDecision <- readMVar otherRematchVar

            if myDecision && otherDecision
            then do
                putStrLn $ "Player " ++ show me ++ " Initiating rematch."
                Control.Monad.when (me == X) $ do
                    -- *** SUA: Don dep MVar de choi lai lan 3 ***
                    _ <- tryTakeMVar myRematchVar
                    _ <- tryTakeMVar otherRematchVar
                    _ <- swapMVar gameStateVar initialGame
                    sendGame hMy gameStateVar
                    sendGame hOther gameStateVar

                playerLoop hMy hOther gameStateVar me myRematchVar otherRematchVar
            else do
                hPutStrLn hMy "QUIT"
                putStrLn $ "Player " ++ show me ++ " Quitting Session."

    -- (Logic gameLoop da dung)
    gameLoop :: IO Bool
    gameLoop = do
        line <- hGetLine hMy

        if line == "GAME_OVER_ACK"
        then return True
        else if "CHAT:" `isPrefixOf` line 
        then do
            hPutStrLn hOther ("MSG: " ++ drop 5 line) 
            gameLoop 
        else do
            modifyMVar_ gameStateVar $ \g -> do
                if gameStatus g /= Running then return g
                else if currentPlayer g /= me then do
                    hPutStrLn hMy "MSG: Not your turn!" 
                    return g
                else do
                    let (finalGame, valid) = applyMove g me line
                    if not valid then do
                        hPutStrLn hMy "MSG: Invalid move." 
                        return g
                    else do
                        hPrint hMy finalGame
                        hPrint hOther finalGame
                        return finalGame
            gameLoop

    handler :: SomeException -> IO ()
    handler e = do
        putStrLn $ "Player " ++ show me ++ " disconnected: " ++ show e
        hPutStrLn hOther "MSG: Opponent disconnected." 
        putMVar myRematchVar False
        return ()

sendGame :: Handle -> MVar Game -> IO ()
sendGame h gameVar = do
    g <- readMVar gameVar
    hPrint h g

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
validMove b r c = r >= 0 && r < boardSize && c >= 0 && c < boardSize && b !! r !! c == Empty

makeMove :: Game -> Player -> Int -> Int -> Game
makeMove g p r c =
    let newBoard = take r (board g)
                    ++ [take c (board g !! r)
                    ++ [Taken p]
                    ++ drop (c + 1) (board g !! r)]
                    ++ drop (r + 1) (board g)
    in g { board = newBoard, currentPlayer = nextPlayer p }

-- *** DAY LA PHIEN BAN SUA LOI CUA `updateGameStatus` VA `findWin` ***

-- 1. Ham chinh: updateGameStatus
updateGameStatus :: Game -> Player -> Int -> Int -> Game
updateGameStatus g lastPlayer r c =
    case findWin (board g) lastPlayer (r, c) of
        Just cells -> g { gameStatus = Win lastPlayer, winningCells = cells }
        Nothing    -> if checkDraw (board g)
                      then g { gameStatus = Draw }
                      else g

-- 2. Ham tim kiem
findWin :: Board -> Player -> (Int, Int) -> Maybe [(Int, Int)]
findWin b p cell =
    let directions = [(1,0), (0,1), (1,1), (1,-1)] -- 4 huong
    in getFirstJust $ map (checkDir b p cell) directions
  where
    -- Ham checkDir (da sua loi `take 5`)
    checkDir :: Board -> Player -> (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
    checkDir b p cell (dr, dc) =
        let line1 = getLineInDir b p cell dr dc
            line2 = getLineInDir b p cell (-dr) (-dc)
            fullLine = (reverse line1) ++ [cell] ++ line2
        in if length fullLine < winCondition
           then Nothing
           else 
            -- Tim tat ca cac "cua so" 5-o lien tiep
            let windows = filter (\w -> length w == winCondition) (map (take winCondition) (tails fullLine))
            -- Tim "cua so" dau tien co chua nuoc di (cell) hien tai
            in find (elem cell) windows

    getFirstJust :: [Maybe a] -> Maybe a
    getFirstJust [] = Nothing
    getFirstJust (Just x : _) = Just x
    getFirstJust (Nothing : xs) = getFirstJust xs

-- 3. Ham de quy tim line
getLineInDir :: Board -> Player -> (Int, Int) -> Int -> Int -> [(Int, Int)]
getLineInDir b p (r, c) dr dc =
    let r' = r + dr
        c' = c + dc
    in if r' < 0 || r' >= boardSize || c' < 0 || c' >= boardSize || (b !! r' !! c' /= Taken p)
       then []
       else (r', c') : getLineInDir b p (r', c') dr dc

-- 4. Ham kiem tra hoa
checkDraw :: Board -> Bool
checkDraw = all (notElem Empty)
