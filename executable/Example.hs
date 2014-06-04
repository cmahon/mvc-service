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
import           Data.Typeable                    
import           MVC
import           MVC.Event                        hiding (handleEvent)
import           MVC.Model                        
import           MVC.Prelude
import           MVC.Service
import qualified Pipes.Prelude                    as P
import           Prelude                          hiding (id,(.))

-----------------------------------------------------------------------------
-- Example 1

msA :: ManagedService String String
msA = toManagedService $ fromPipe Unbounded Unbounded $ forever $ await >>= \e -> when (e == "do") $ yield "done" 

msB :: ManagedService String String
msB = toManagedService $ fromPipe Unbounded Unbounded $ forever $ await >>= \e -> when (e == "done") $ yield $ e ++ " (test2)"

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

-- Services: SomeEvent-wrapped stdin & stdout

instance Event String

external' :: Managed (View SomeEvent, Controller SomeEvent)
external' = do
  c <- stdinLines
  return (contramap show stdoutLines,fmap SomeEvent c)

-- App Service: Incrementer

data TestAppService a = TestAppService
  { _testCount :: Int
  }

newTestAppService :: AppStateAPI (TestAppService a) -> SomeAppService a
newTestAppService api = SomeAppService 0 api (TestAppService 0)

instance AppService (TestAppService a) where
  type AppState (TestAppService a) = a
  data AppStateAPI (TestAppService a) = TestAppServiceAPI { _testTest :: a -> a}
  processEvent _ e
    | Just ("inc"::String) <- fromEvent e = do
        api <- getAppStateAPI
        modifyAppState (_testTest api)
        noEvents
    | otherwise = noEvents

-- App Service: Logger

data Msg = Msg String deriving (Typeable,Show)

instance Event Msg

data LogAppService a = LogAppService

instance AppService (LogAppService a) where
  type AppState (LogAppService a) = a
  data AppStateAPI (LogAppService a) = LogAppServiceAPI
  processEvent _ e = return [release . Msg $ show e]

newLogAppService :: SomeAppService a
newLogAppService = SomeAppService 0 LogAppServiceAPI LogAppService

-- App services

appServices :: [SomeAppService Int]
appServices = initialiseAppServices
  [ newTestAppService (TestAppServiceAPI (+1)) 
  , newLogAppService
  ]

-- Model

model' :: Model Int SomeEvent SomeEvent
model' = asPipe (P.takeWhile (not . done)) >>> asPipe (runAppModel appServices handleEvent)
  where 
  done e
    | Just ("quit" :: String) <- fromEvent e = True 
    | otherwise = False

-- -----------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  print =<< runMVC () model external
  print =<< runMVC 0 model' external'
