module Main where

import           MinMax
import           Chess
import           Fen

import           Control.Concurrent
import           Data.IORef
import           System.IO

max_depth = 4
version = "1.0.0"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    ref <- newIORef "a good move"
    main' ref (readFen fen_start) Nothing

main' :: IORef String -> Chess -> Maybe ThreadId -> IO ()
main' ref currentPosition searchThread = do
    line <- getLine
    case words line of
        "uci":_ -> do
            putStrLn $ "id name curryfish (" ++ version ++ ")\nauthor Petter Daae\nuciok"
            main' ref (readFen fen_start) searchThread
        "ucinewgame":_ -> do
            stopSearch searchThread
            main' ref (readFen fen_start) Nothing
        "go":_ -> do
            stopSearch searchThread
            tid <- forkIO $ go ref currentPosition 1
            main' ref currentPosition $ Just tid
        "stop":_ -> do
            stopSearch searchThread
            bestMove <- readIORef ref
            putStrLn $ "bestmove " ++ bestMove
            main' ref currentPosition searchThread
        "quit":_    -> return ()
        "isready":_ -> do
            putStrLn "readyok"
            main' ref currentPosition searchThread
        "position":xs -> do
            stopSearch searchThread
            main' ref (readUCIPosition xs) searchThread-- position [fen  | startpos ]  moves  .... 
        "printboard":_ -> do
            print currentPosition
            main' ref currentPosition searchThread
        _          -> main' ref currentPosition searchThread

go :: IORef String -> Chess -> Int -> IO ()
go ref chess depth =
    let bestMove = showMove $ minmax chess depth
    in  do
            modifyIORef' ref (\_ -> bestMove)
            putStrLn $ "info depth " ++ (show depth)
            if depth == max_depth then putStrLn $ "bestmove " ++ bestMove
            else go ref chess (depth + 1)

stopSearch :: Maybe ThreadId -> IO ()
stopSearch (Just id) = killThread id
stopSearch Nothing   = return ()
