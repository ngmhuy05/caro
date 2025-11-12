-- src/Client.hs
-- -----------------------------------------------------------------------------
-- Client entrypoint.
-- Nhiệm vụ:
--   1) Đọc host/port từ argv: [host] [port] (mặc định 127.0.0.1:4444)
--   2) Kết nối TCP, nhận "PLAYER: X|O"
--   3) Bàn giao socket handle + vai trò cho GUI
-- Không sửa đổi hành vi so với bản gốc.
-- -----------------------------------------------------------------------------
module Main where

import Network.Socket
import System.IO ( hSetEncoding, utf8
                 , hSetBuffering, BufferMode(LineBuffering)
                 , IOMode(ReadWriteMode), stdout, hGetLine )
import System.Environment (getArgs)
import GameState (Player(..))
import GUI (runGUI)

main :: IO ()
main = withSocketsDo $ do
  -- Chuẩn hoá stdout để log Unicode, tránh buffer dồn
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8

  -- argv: [server_ip] [port]
  let pick def f xs = case xs of (x:_) | not (null x) -> f x; _ -> def
  args <- getArgs
  let host = pick "127.0.0.1" id args
      port = pick "4444"       id (drop 1 args)

  -- Resolve + kết nối
  let hints = defaultHints { addrSocketType = Stream, addrFamily = AF_INET }
  ais <- getAddrInfo (Just hints) (Just host) (Just port)
  let ai = head ais
  sock <- socket (addrFamily ai) (addrSocketType ai) defaultProtocol
  connect sock (addrAddress ai)

  -- Dùng Handle để đọc/ghi dòng
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h LineBuffering
  hSetEncoding h utf8

  -- Nhận vai trò
  playerMsg <- hGetLine h             -- "PLAYER: X" | "PLAYER: O"
  let me = read (drop 8 playerMsg) :: Player

  -- Chạy vòng GUI (Gloss)
  runGUI h me
