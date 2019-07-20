module Main where

import           MinMax
import           Chess
import           Fen

import           Control.Concurrent
import           Data.IORef
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    ref <- newIORef "a good move"
    main' ref (readFen fen_start) Nothing

main' :: IORef String -> Chess -> Maybe ThreadId -> IO ()
main' ref currentPosition searchThread = do
    line <- getLine
    case line of
        "uci" -> do
            putStrLn "id name curryfish\nauthor Petter Daae\nuciok"
            main' ref (readFen fen_start) searchThread
        "ucinewgame" -> do
            stopSearch searchThread
            main' ref (readFen fen_start) Nothing
        ('g' : 'o' : xs) -> do
            stopSearch searchThread
            tid <- forkIO $ go ref currentPosition 1
            main' ref currentPosition $ Just tid
        "stop" -> do
            stopSearch searchThread
            bestMove <- readIORef ref
            putStrLn $ "bestmove " ++ bestMove
            main' ref currentPosition searchThread
        "quit"    -> return ()
        "isready" -> do
            putStrLn "readyok"
            main' ref currentPosition searchThread
        "position" -> undefined -- position [fen  | startpos ]  moves  .... 
        _          -> main' ref currentPosition searchThread

go :: IORef String -> Chess -> Int -> IO ()
go ref chess depth =
    let bestMove = showMove $ minmax chess depth
    in  do
            putStrLn $ "info depth " ++ (show depth)
            modifyIORef' ref (\_ -> bestMove)
            go ref chess (depth + 1)

stopSearch :: Maybe ThreadId -> IO ()
stopSearch (Just id) = killThread id
stopSearch Nothing   = return ()
