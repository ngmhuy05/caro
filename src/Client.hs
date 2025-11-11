-- src/Client.hs
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
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8

  -- argv: [server_ip] [port]
  args <- getArgs
  let host = case args of
               (h:_) | not (null h) -> h
               _                    -> "127.0.0.1"
      port = case args of
               (_:p:_) | not (null p) -> p
               _                      -> "4444"

  let hints = defaultHints { addrSocketType = Stream, addrFamily = AF_INET }
  ais <- getAddrInfo (Just hints) (Just host) (Just port)
  let ai = head ais
  sock <- socket (addrFamily ai) (addrSocketType ai) defaultProtocol
  connect sock (addrAddress ai)

  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h LineBuffering
  hSetEncoding h utf8

  playerMsg <- hGetLine h            -- "PLAYER: X" hoặc "PLAYER: O"
  let me = read (drop 8 playerMsg) :: Player
  runGUI h me
