-- File: Client.hs
{-# LANGUAGE OverloadedStrings #-}
import Network.Socket
import System.IO (hSetEncoding, utf8, Handle, hSetBuffering, BufferMode(LineBuffering), IOMode(ReadWriteMode), stdout, hGetLine)
import GameState (Player(..))
import Data.Char (toUpper)

import GUI (runGUI)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetEncoding stdout utf8
    
    -- 1. Connect to server
    sock <- socket AF_INET Stream 0
    connect sock (SockAddrInet 4444 (tupleToHostAddress (127, 0, 0, 1)))
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h LineBuffering
    hSetEncoding h utf8
    putStrLn "Connected to server."

    -- 2. Get Player info
    playerMsg <- hGetLine h
    let me = read (drop 8 playerMsg) :: Player
    putStrLn $ "You are player: " ++ show me

    -- 3. Run GUI and transfer control
    runGUI h me
    
    putStrLn "Game over. Closing connection."
