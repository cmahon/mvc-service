module Main where

import Control.Concurrent
import System.IO
import Network
import Control.Monad
import Text.Printf

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
    (hdl, host, port') <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port')
    forkFinally (talk hdl) (\_ -> hClose hdl)

port :: Int
port = 44444

talk :: Handle -> IO ()
talk hdl = do
  hSetBuffering hdl LineBuffering
  hPutStrLn hdl "Hello"
  forever $ do
    i <- hGetLine hdl
    hPutStrLn hdl ("server: " ++ i)