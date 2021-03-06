module Main where

import           MinMax
import           Chess
import           Fen

import           Control.Concurrent
import           Data.IORef
import           System.IO

version = "1.0.0"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    ref <- newIORef "a good move"
    main' ref (readFen fen_start) Nothing Nothing

main' :: IORef String -> Chess -> Maybe ThreadId -> Maybe ThreadId -> IO ()
main' ref currentPosition searchThread moveTimeThread = do
    line <- getLine
    case words line of
        "uci":_ -> do
            putStrLn $ "id name curryfish (" ++ version ++ ")\nauthor Petter Daae\nuciok"
            main' ref (readFen fen_start) searchThread moveTimeThread
        "ucinewgame":_ -> do
            stopThread moveTimeThread
            stopThread searchThread
            main' ref (readFen fen_start) Nothing Nothing
        "go":"movetime":time:_ -> do
            stopThread moveTimeThread
            stopThread searchThread
            tid <- forkIO $ go ref currentPosition 1
            mid <- forkIO $ do
                threadDelay $ (read time) * 1000
                stopThread (Just tid)
                bestMove <- readIORef ref
                putStrLn $ "bestmove " ++ bestMove
            main' ref currentPosition (Just tid) (Just mid)
        "go":_ -> do
            stopThread searchThread
            stopThread moveTimeThread
            tid <- forkIO $ go ref currentPosition 1
            main' ref currentPosition (Just tid) Nothing
        "stop":_ -> do
            stopThread moveTimeThread
            stopThread searchThread
            bestMove <- readIORef ref
            putStrLn $ "bestmove " ++ bestMove
            main' ref currentPosition Nothing Nothing
        "quit":_    -> return ()
        "isready":_ -> do
            putStrLn "readyok"
            main' ref currentPosition searchThread moveTimeThread
        "position":xs -> do
            stopThread moveTimeThread
            stopThread searchThread
            main' ref (readUCIPosition xs) Nothing Nothing
        "printboard":_ -> do
            print currentPosition
            main' ref currentPosition searchThread moveTimeThread
        _          -> main' ref currentPosition searchThread moveTimeThread

go :: IORef String -> Chess -> Int -> IO ()
go ref chess depth =
    let bestMove = showMove $ minmax chess depth
    in  do
            modifyIORef' ref (\_ -> bestMove)
            putStrLn $ "info depth " ++ (show depth)
            go ref chess (depth + 1)

stopThread :: Maybe ThreadId -> IO ()
stopThread (Just id) = killThread id
stopThread Nothing   = return ()
