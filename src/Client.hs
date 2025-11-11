-- File: Client.hs
{-# LANGUAGE OverloadedStrings #-}
import Network.Socket
import System.IO (hSetEncoding, utf8, hSetBuffering, BufferMode(LineBuffering), IOMode(ReadWriteMode), stdout, hGetLine)
import GameState (Player(..))
import GUI (runGUI)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  sock <- socket AF_INET Stream 0
  connect sock (SockAddrInet 4444 (tupleToHostAddress (127, 0, 0, 1)))
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h LineBuffering
  hSetEncoding h utf8
  putStrLn "Connected to server."
  playerMsg <- hGetLine h
  let me = read (drop 8 playerMsg) :: Player
  putStrLn $ "You are player: " ++ show me
  runGUI h me
  putStrLn "Game over. Closing connection."
