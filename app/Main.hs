module Main where

import           Control.Concurrent
import           Data.IORef

main :: IO ()
main = do
    ref <- newIORef "a good move"
    main' ref

main' :: IORef String -> IO ()
main' ref = do
    line <- getLine
    case line of
        "uci" -> do
            putStrLn "id name curryfish\nauthor Petter Daae\nuciok"
            main' ref
        "ucinewgame" -> main' ref
        "go"         -> do
            forkIO $ go ref
            main' ref
        "stop" -> do
            bestMove <- readIORef ref
            putStrLn bestMove
            main' ref
        "quit" -> return ()
        _      -> main' ref

go :: IORef String -> IO ()
go ref = do
    threadDelay 5000000
    modifyIORef' ref $ const "a better move"
