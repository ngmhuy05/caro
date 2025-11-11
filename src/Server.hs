-- src/Server.hs
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Socket
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import System.IO
import GameState
import Data.List (isPrefixOf)
import Control.Exception (catch, SomeException, try, IOException)
import Control.Monad (when)
import Data.Maybe (listToMaybe)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bindAnyOrLoopback sock 4444
  listen sock 2
  putStrLn "Server online, waiting for connections..."
  mainLoop sock

-- bind 0.0.0.0, nếu lỗi thì 127.0.0.1
bindAnyOrLoopback :: Socket -> PortNumber -> IO ()
bindAnyOrLoopback sock port = do
  let anyAddr  = SockAddrInet port (tupleToHostAddress (0,0,0,0))
      loopback = SockAddrInet port (tupleToHostAddress (127,0,0,1))
  r <- try (bind sock anyAddr) :: IO (Either IOException ())
  case r of
    Right _ -> pure ()
    Left _  -> bind sock loopback

mainLoop :: Socket -> IO ()
mainLoop sock = do
  (c1, _) <- accept sock
  h1 <- socketToHandle c1 ReadWriteMode
  hSetBuffering h1 LineBuffering
  hSetEncoding h1 utf8
  hPutStrLn h1 "PLAYER: X"
  hPutStrLn h1 "MSG: Welcome player (X). Waiting for opponent..."

  (c2, _) <- accept sock
  h2 <- socketToHandle c2 ReadWriteMode
  hSetBuffering h2 LineBuffering
  hSetEncoding h2 utf8
  hPutStrLn h2 "PLAYER: O"
  hPutStrLn h2 "MSG: Welcome player (O)."

  putStrLn "Pair matched! Starting game."

  gv <- newMVar initialGame
  rx <- newEmptyMVar :: IO (MVar Bool)
  ro <- newEmptyMVar :: IO (MVar Bool)

  sendGame h1 gv
  sendGame h2 gv
  _ <- forkIO (playerLoop h1 h2 gv X rx ro)
  _ <- forkIO (playerLoop h2 h1 gv O ro rx)
  mainLoop sock

playerLoop :: Handle -> Handle -> MVar Game -> Player -> MVar Bool -> MVar Bool -> IO ()
playerLoop hMy hOther gv me mMine mOther = session `catch` handler
  where
    session = do
      ended <- loop
      when ended $ do
        threadDelay 3000000
        hPutStrLn hMy "REMATCH? Y/N"
        d <- hGetLine hMy
        let mine = d == "Y"
        putMVar mMine mine
        other <- readMVar mOther
        if mine && other
          then do
            when (me == X) $ do
              _ <- tryTakeMVar mMine
              _ <- tryTakeMVar mOther
              _ <- swapMVar gv initialGame
              sendGame hMy gv
              sendGame hOther gv
            playerLoop hMy hOther gv me mMine mOther
          else hPutStrLn hMy "QUIT"

    loop :: IO Bool
    loop = do
      ln <- hGetLine hMy
      if ln == "GAME_OVER_ACK"
        then return True
        else if "CHAT:" `isPrefixOf` ln
          then hPutStrLn hOther ("MSG: " ++ drop 5 ln) >> loop
          else do
            modifyMVar_ gv $ \g ->
              if gameStatus g /= Running then return g
              else if currentPlayer g /= me
                then hPutStrLn hMy "MSG: Not your turn!" >> return g
                else do
                  let (g', ok) = applyMove g me ln
                  if not ok
                    then hPutStrLn hMy "MSG: Invalid move." >> return g
                    else hPrint hMy g' >> hPrint hOther g' >> return g'
            loop

    handler :: SomeException -> IO ()
    handler _ = hPutStrLn hOther "MSG: Opponent disconnected." >> putMVar mMine False

sendGame :: Handle -> MVar Game -> IO ()
sendGame h gv = readMVar gv >>= hPrint h

applyMove :: Game -> Player -> String -> (Game, Bool)
applyMove g me ln =
  case words ln of
    [rS, cS] ->
      case (reads rS, reads cS) of
        ([(r,"")], [(c,"")]) ->
          let r0 = r - 1
              c0 = c - 1
          in if r0>=0 && r0<boardSize && c0>=0 && c0<boardSize && board g !! r0 !! c0 == Empty
                then (updateGameStatus (place g me r0 c0) me r0 c0, True)
                else (g, False)
        _ -> (g, False)
    _ -> (g, False)

place :: Game -> Player -> Int -> Int -> Game
place g p r c =
  let row  = board g !! r
      row' = take c row ++ [Taken p] ++ drop (c+1) row
      b'   = take r (board g) ++ [row'] ++ drop (r+1) (board g)
  in g { board = b', currentPlayer = nextPlayer p }

updateGameStatus :: Game -> Player -> Int -> Int -> Game
updateGameStatus g p r c =
  case findWin (board g) p (r,c) of
    Just cells -> g { gameStatus = Win p, winningCells = cells }
    Nothing    -> if all (notElem Empty) (board g)
                    then g { gameStatus = Draw }
                    else g

findWin :: Board -> Player -> (Int,Int) -> Maybe [(Int,Int)]
findWin b p cell =
  let dirs = [(1,0),(0,1),(1,1),(1,-1)]
  in listToMaybe $ mapMaybe (checkDir b p cell) dirs
  where
    mapMaybe _ [] = []
    mapMaybe f (x:xs) = case f x of
      Nothing -> mapMaybe f xs
      Just y  -> y : mapMaybe f xs

checkDir :: Board -> Player -> (Int,Int) -> (Int,Int) -> Maybe [(Int,Int)]
checkDir b p (r,c) (dr,dc) =
  let go rr cc drr dcc acc =
        let r' = rr + drr
            c' = cc + dcc
        in if r'<0 || r'>=boardSize || c'<0 || c'>=boardSize || b !! r' !! c' /= Taken p
             then acc
             else go r' c' drr dcc ((r',c'):acc)
      neg  = go r c (-dr) (-dc) []
      pos  = reverse (go r c dr dc [])
      full = neg ++ [(r,c)] ++ pos
  in if length full < winCondition
       then Nothing
       else Just (take winCondition (drop (max 0 (length neg - (winCondition-1) `div` 2)) full))
