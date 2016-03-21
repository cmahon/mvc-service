{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.Category
import           Control.Monad  
import           Data.Monoid  
import           MVC
import           MVC.Event                        hiding (handleEvent)
import           MVC.EventHandler
import           MVC.Prelude
import           MVC.Service
import qualified Pipes.Prelude                    as P
import           Prelude                          hiding (id,(.))

-----------------------------------------------------------------------------
-- Example 1

msA :: ManagedService String String
msA = toManagedService $ fromPipe unbounded unbounded $ forever $ await >>= \e -> when (e == "do") $ yield "done" 

msB :: ManagedService String String
msB = toManagedService $ fromPipe unbounded unbounded $ forever $ await >>= \e -> when (e == "done") $ yield $ e ++ " (test2)"

external :: Managed (View String, Controller String)
external = do
  (v1,c1) <- toManagedMVC $ msA >>> msB
  c2 <- stdinLines
  (v3,c3) <- toManagedMVC $ msA <> msB
  return (v1 <> stdoutLines <> v3, mconcat [c1,c2,c3])

model :: Model () String String
model = asPipe (P.takeWhile (/= "quit"))

-----------------------------------------------------------------------------
-- Example 2

external' :: Managed (View SomeEvent, Controller SomeEvent)
external' = do
  c <- stdinLines
  return (contramap show stdoutLines,fmap SomeEvent c)

data TestEventHandler a = TestEventHandler
  { _testCount :: Int
  }

newTestEventHandler :: AppStateAPI (TestEventHandler a) -> EventHandler SomeEvent SomeEvent a
newTestEventHandler api = mkEventHandler $ SomeEventHandler 0 api Just id (TestEventHandler 0)

instance HandlesEvent (TestEventHandler a) where
  type AppState (TestEventHandler a) = a
  type EventIn (TestEventHandler a) = SomeEvent
  type EventOut (TestEventHandler a) = SomeEvent
  data AppStateAPI (TestEventHandler a) = TestEventHandlerAPI
    { _testQuery :: a -> Int
    , _testModify :: a -> a
    }
  handleEvent _ e
    | Just ("inc"::String) <- fromEvent e = do
        api <- getAppStateAPI
        v <- getsAppState (_testQuery api)
        modifyAppState (_testModify api)
        v' <- getsAppState (_testQuery api)
        return
          [ release' . Msg $ "state pre inc: " ++ show v
          , release' . Msg $ "state post inc: " ++ show v'
          ]
    | Just ("id"::String) <- fromEvent e = do
        i <- getEventHandlerId
        return [release' . Msg $ "App service id: " ++ show i]
    | otherwise = noEvents

eventHandler :: EventHandler SomeEvent SomeEvent Int
eventHandler = initialiseEventHandler $ mconcat
  [ newLogEventHandler (Just . show) toEitherSomeEvent
  , newTestEventHandler (TestEventHandlerAPI id (+1))
  ]

model' :: Model Int SomeEvent SomeEvent
model' = asPipe (P.takeWhile (not . done)) >>> asPipe (runRecursiveEventHandler eventHandler)
  where 
  done e
    | Just ("quit" :: String) <- fromEvent e = True 
    | otherwise = False

-----------------------------------------------------------------------------
-- Example 3

external'' :: Managed (View Msg, Controller String)
external'' = do
  c <- stdinLines
  return (contramap show stdoutLines,c)

newTestEventHandler' :: AppStateAPI (TestEventHandler a) -> EventHandler String Msg a
newTestEventHandler' api = mkEventHandler $ SomeEventHandler 0 api (Just . SomeEvent) fromEitherSomeEvent (TestEventHandler 0)

eventHandler' :: EventHandler String Msg Int
eventHandler' = initialiseEventHandler $ mconcat
  [ newLogEventHandler (Just . show) id
  , newTestEventHandler' (TestEventHandlerAPI id (+1))
  ]

model'' :: Model Int String Msg
model'' = asPipe (P.takeWhile (not . done)) >>> asPipe (runRecursiveEventHandler eventHandler')
  where 
  done "quit" = True
  done _ = False

-- -----------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  print =<< runMVC () model external
  print =<< runMVC 0 model' external'
  print =<< runMVC 0 model'' external''


