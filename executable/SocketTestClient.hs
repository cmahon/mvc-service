{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent        (threadDelay)
import           Control.Exception
import           Control.Lens
import           Data.ByteString.Char8     (isInfixOf)
import           Data.Default
import           MVC
import           MVC.Prelude
import           MVC.Service
import           System.IO                 (stdout)
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger

import           MVC.Socket

-----------------------------------------------------------------------------

model :: Model () ServiceOut (Either ServiceIn String)
model = asPipe $ do
  
  sendEvent $ ServiceCommand ServiceReportStatus
  
  sendEvent $ ConnectionCommand $ Connect 0
  
  onCondition (== ServiceStatus ServiceActive) $
    sendEvent $ Send "hello1\n"
  
  onCondition (streamContains "server: hello1\n") $
    sendEvent $ ConnectionCommand Disconnect
  
  onCondition (== ServiceStatus ServicePending) $
    sendEvent $ ConnectionCommand $ Connect 0
  
  onCondition (== ServiceStatus ServiceActive) $
    sendEvent $ Send "hello2\n"
  
  for cat logEvent
  
  where
  
  sendEvent = yield . Left

  logEvent = yield . Right . show

  streamContains bs = \case 
    Stream x -> bs `isInfixOf` x
    _ -> False

  onCondition cond action = go
    where
    go = do
      e <- await
      logEvent e
      if cond e
        then action
        else go

-----------------------------------------------------------------------------

services :: Managed (View (Either ServiceIn String), Controller ServiceOut)
services = do
  (socketV,socketC) <- toManagedMVC $ toManagedService $ socketService $ 
    def & scSocketParams .~ SocketParams "127.0.0.1" "44444"
  return
    ( handles _Left socketV <> handles _Right stdoutLines
    , socketC
    )

-----------------------------------------------------------------------------

main :: IO ()
main = do
  handler <- streamHandler stdout DEBUG >>= \h -> return $
   setFormatter h $ simpleLogFormatter "$time $loggername $prio: $msg"
  updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [handler])
  finally (runMVC () model services) (threadDelay 1000000)


