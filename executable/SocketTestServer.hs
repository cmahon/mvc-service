module Main where

import Control.Concurrent
import Control.Concurrent.Async
import System.IO
import Control.Exception
import Network
import Control.Monad
import Text.Printf

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (talk handle) (\_ -> hClose handle)

port :: Int
port = 44444

talk :: Handle -> IO ()
talk handle = do
  hSetBuffering handle LineBuffering
  hPutStrLn handle "Hello"
  forever $ do
    i <- hGetLine handle
    hPutStrLn handle ("server: " ++ i)