-- src/Server.hs
-- -----------------------------------------------------------------------------
-- TCP server ghép cặp 2 client rồi điều phối ván cờ.
-- Đặc điểm:
--   - Gửi/nhận theo dòng; chống sập khi client rời (safePut/safePrint/safeClose).
--   - Vòng mainLoop ghép liên tục nhiều cặp.
--   - Trả về winningCells 0-based đầy đủ 5 ô (nếu thắng).
-- Giữ nguyên thuật toán; chỉ chú thích và dọn layout.
-- -----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Socket
import System.IO
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (SomeException, IOException, catch, try)
import Control.Monad (when)
import System.Environment (getArgs)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)

import GameState

-- === Safe IO helpers =========================================================
safePut :: Handle -> String -> IO ()
safePut h s = (hPutStrLn h s >> hFlush h) `catch` (\(_ :: IOException) -> pure ())

safePrint :: Handle -> Game -> IO ()
safePrint h g = (hPrint h g >> hFlush h) `catch` (\(_ :: IOException) -> pure ())

safeClose :: Handle -> IO ()
safeClose h = hClose h `catch` (\(_ :: IOException) -> pure ())

-- =============================================================================

main :: IO ()
main = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8

  -- Lấy host/port từ argv, mặc định 0.0.0.0:4444
  args <- getArgs
  let bindHost = case args of (h:_)   | not (null h) -> h; _ -> "0.0.0.0"
      bindPort = case args of (_:p:_) | not (null p) -> p; _ -> "4444"

  sock <- setupListener bindHost bindPort
  putStrLn $ "Server online, listening on " ++ bindHost ++ ":" ++ bindPort
  mainLoop sock

-- Tạo socket lắng nghe; fallback 127.0.0.1 khi bind 0.0.0.0 lỗi (Windows edge case)
setupListener :: String -> String -> IO Socket
setupListener host port = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream, addrFamily = AF_INET }
  ais <- getAddrInfo (Just hints) (Just host) (Just port)
  let ai = head ais
  sock <- socket (addrFamily ai) (addrSocketType ai) defaultProtocol
  setSocketOption sock ReuseAddr 1
  r <- try (bind sock (addrAddress ai)) :: IO (Either IOException ())
  case r of
    Right _ -> listen sock 16 >> pure sock
    Left _  -> do
      lais <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just port)
      let lai = head lais
      bind sock (addrAddress lai)
      listen sock 16
      pure sock

-- Ghép 2 client thành 1 cặp rồi chạy 2 thread playerLoop; sau đó quay lại nhận cặp mới
mainLoop :: Socket -> IO ()
mainLoop sock = do
  (c1, _) <- accept sock
  h1 <- socketToHandle c1 ReadWriteMode
  hSetBuffering h1 LineBuffering >> hSetEncoding h1 utf8
  safePut h1 "PLAYER: X"
  safePut h1 "MSG: Welcome player (X). Waiting for opponent..."

  (c2, _) <- accept sock
  h2 <- socketToHandle c2 ReadWriteMode
  hSetBuffering h2 LineBuffering >> hSetEncoding h2 utf8
  safePut h2 "PLAYER: O"
  safePut h2 "MSG: Welcome player (O)."

  putStrLn "Pair matched! Starting game."

  gv <- newMVar initialGame
  rx <- newEmptyMVar :: IO (MVar Bool)
  ro <- newEmptyMVar :: IO (MVar Bool)

  sendGame h1 gv
  sendGame h2 gv
  _ <- forkIO (playerLoop h1 h2 gv X rx ro)
  _ <- forkIO (playerLoop h2 h1 gv O ro rx)

  -- Không chờ cặp hiện tại kết thúc; tiếp tục chấp nhận cặp mới
  mainLoop sock

-- Vòng xử lý cho mỗi người chơi của một cặp
playerLoop :: Handle -> Handle -> MVar Game -> Player -> MVar Bool -> MVar Bool -> IO ()
playerLoop hMy hOther gv me mMine mOther = session `catch` handler
  where
    -- session: lắng nghe cho đến khi ván kết thúc, sau đó hỏi rematch
    session = do
      ended <- loop
      when ended $ do
        threadDelay 3000000
        safePut hMy "REMATCH? Y/N"
        d <- (hGetLine hMy `catch` (\(_ :: IOException) -> pure "N"))
        let mine = d == "Y"
        putMVar mMine mine
        other <- readMVar mOther
        if mine && other
          then do
            -- Chỉ X reset ván (tránh race)
            when (me == X) $ do
              _ <- tryTakeMVar mMine
              _ <- tryTakeMVar mOther
              _ <- swapMVar gv initialGame
              sendGame hMy gv
              sendGame hOther gv
            playerLoop hMy hOther gv me mMine mOther
          else do
            safePut hMy "QUIT"
            safeClose hMy
            -- Không đóng hOther ở đây; thread bên kia tự xử

    -- loop: đọc lệnh; CHAT: chuyển tiếp; nước đi -> cập nhật và broadcast
    loop :: IO Bool
    loop = do
      lnE <- try (hGetLine hMy) :: IO (Either IOException String)
      case lnE of
        Left _ -> pure True
        Right ln ->
          if ln == "GAME_OVER_ACK"
            then return True
            else if "CHAT:" `isPrefixOf` ln
              then safePut hOther ("MSG: " ++ drop 5 ln) >> loop
              else do
                modifyMVar_ gv $ \g ->
                  if gameStatus g /= Running then return g
                  else if currentPlayer g /= me
                    then safePut hMy "MSG: Not your turn!" >> return g
                    else do
                      let (g', ok) = applyMove g me ln
                      if not ok
                        then safePut hMy "MSG: Invalid move." >> return g
                        else safePrint hMy g' >> safePrint hOther g' >> return g'
                loop

    -- Khi client rời: báo bên kia và đóng hMy
    handler :: SomeException -> IO ()
    handler _ = do
      safePut hOther "MSG: Opponent disconnected."
      putMVar mMine False
      safeClose hMy

-- Gửi trạng thái hiện tại cho 1 handle
sendGame :: Handle -> MVar Game -> IO ()
sendGame h gv = readMVar gv >>= safePrint h

-- Phân tích chuỗi "r c" 1-based, kiểm tra hợp lệ, cập nhật ván
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

-- Đặt quân lên (0-based) và đổi lượt
place :: Game -> Player -> Int -> Int -> Game
place g p r c =
  let b    = board g
      row  = b !! r
      row' = take c row ++ [Taken p] ++ drop (c + 1) row
      b'   = take r b ++ [row'] ++ drop (r + 1) b
  in g { board = b', currentPlayer = nextPlayer p }

-- Sau khi đặt quân: kiểm tra thắng/hoà; nếu thắng lưu dãy winningCells (0-based)
updateGameStatus :: Game -> Player -> Int -> Int -> Game
updateGameStatus g p r c =
  case findWin (board g) p (r,c) of
    Just cells -> g { gameStatus = Win p, winningCells = cells }
    Nothing    -> if all (notElem Empty) (board g)
                    then g { gameStatus = Draw }
                    else g

-- Tìm dãy 5 liên tiếp theo 4 hướng; trả về Nothing nếu chưa đủ
findWin :: Board -> Player -> (Int,Int) -> Maybe [(Int,Int)]
findWin b p cell =
  let dirs = [(1,0),(0,1),(1,1),(1,-1)]
  in listToMaybe [ seg | d <- dirs, Just seg <- [checkDir b p cell d] ]

-- Kiểm tra theo 1 hướng (dr,dc), mở rộng tối đa qua 2 phía quanh ô mới đánh
checkDir :: Board -> Player -> (Int,Int) -> (Int,Int) -> Maybe [(Int,Int)]
checkDir b p (r,c) (dr,dc) =
  let grow rr cc drr dcc acc =
        let r' = rr + drr; c' = cc + dcc
        in if r'<0 || r'>=boardSize || c'<0 || c'>=boardSize || b !! r' !! c' /= Taken p
             then acc
             else grow r' c' drr dcc ((r',c') : acc)

      neg  = grow r c (-dr) (-dc) []     -- phía âm (gần -> xa)
      posR = grow r c   dr    dc  []     -- phía dương (gần -> xa)
      pos  = reverse posR
      full = reverse neg ++ [(r,c)] ++ pos

      nNeg  = length neg
      total = length full
  in if total < winCondition
       then Nothing
       else
         -- Cửa sổ đủ 5 phần tử, đảm bảo chứa (r,c)
         let start = max 0 (nNeg - (winCondition - 1))
             seg   = take winCondition (drop start full)
         in Just seg
