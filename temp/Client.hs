{-# LANGUAGE OverloadedStrings #-}
import Network.Socket
import System.IO (hSetEncoding, utf8, Handle, hSetBuffering, BufferMode(LineBuffering), hPutStrLn, hGetLine, IOMode(ReadWriteMode), stdout)
import GameState (Player(..), Game(..), GameStatus(..), Cell(..), boardSize, Board)
import Data.Char (toUpper)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    
    sock <- socket AF_INET Stream 0
    connect sock (SockAddrInet 4444 (tupleToHostAddress (127, 0, 0, 1)))
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h LineBuffering
    hSetEncoding h utf8
    putStrLn "Da ket noi toi server."

    playerMsg <- hGetLine h
    let me = read (drop 8 playerMsg) :: Player
    putStrLn $ "Ban la nguoi choi: " ++ show me

    gameLoop h me

gameLoop :: Handle -> Player -> IO ()
gameLoop h me = do
    serverMsg <- hGetLine h

    case serverMsg of
        "REMATCH? Y/N" -> do
            putStrLn "--- Tran dau da ket thuc ---"
            reply <- getRematchInput
            hPutStrLn h reply
            gameLoop h me
            
        "QUIT" -> do
            putStrLn "Doi thu da thoat. Ket thuc game."
            return ()
            
        _ | take 3 serverMsg == "MSG" -> do
            putStrLn serverMsg
            gameLoop h me
            
        _ -> do
            let game = read serverMsg :: Game
            printBoard (board game)
            
            case gameStatus game of
                Win player -> do
                    putStrLn $ "--- GAME OVER ---"
                    putStrLn $ "Nguoi choi " ++ show player ++ " da thang!"
                    if currentPlayer game == me
                    then hPutStrLn h "GAME_OVER_ACK"
                    else return ()
                
                Draw -> do
                    putStrLn $ "--- GAME OVER ---"
                    putStrLn $ "Tran dau hoa!"
                    hPutStrLn h "GAME_OVER_ACK"

                Running -> do
                    putStrLn $ "Luot cua: " ++ show (currentPlayer game)
                    if currentPlayer game == me
                    then do
                        move <- getValidMove
                        hPutStrLn h move
                    else do
                        putStrLn "Dang cho doi thu..."
            
            gameLoop h me

getRematchInput :: IO String
getRematchInput = do
    putStrLn "Ban co muon choi lai khong? (y/n)"
    line <- getLine
    case line of
        "y" -> return "Y"
        "Y" -> return "Y"
        "n" -> return "N"
        "N" -> return "N"
        _   -> do
            putStrLn "Nhap khong hop le. Vui long chi nhap 'y' hoac 'n'."
            getRematchInput

getValidMove :: IO String
getValidMove = do
    putStrLn $ "Nhap nuoc di (hang cot), vi du: 1 1 (trong khoang 1 den " ++ show boardSize ++ ")"
    line <- getLine
    case words line of
        [rStr, cStr] ->
            case ( (reads rStr :: [(Int, String)]) , (reads cStr :: [(Int, String)]) ) of
                ([(r, "")], [(c, "")]) -> 
                    if r >= 1 && r <= boardSize && c >= 1 && c <= boardSize
                    then return line
                    else invalidRange
                _ -> invalidFormat
        _ -> invalidFormat
  where
    invalidFormat = do
        putStrLn "ERROR: Dinh dang khong hop le. Vui long nhap 2 so (hang cot)."
        getValidMove
    invalidRange = do
        putStrLn $ "ERROR: Nuoc di phai trong khoang 1 den " ++ show boardSize ++ "."
        getValidMove

printBoard :: Board -> IO ()
printBoard b = do
    putStrLn "Ban co hien tai:"
    mapM_ putStrLn [rowToStr row | row <- b]
  where
    rowToStr :: [Cell] -> String
    rowToStr = unwords . map cellToStr

    cellToStr :: Cell -> String
    cellToStr Empty      = "."
    cellToStr (Taken X)  = "X"
    cellToStr (Taken O)  = "O"