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
import           Data.Maybe                       (fromJust)         
import           Data.Monoid  
import           Data.Typeable                    
import           MVC
import           MVC.Event                        hiding (handleEvent)
import           MVC.Model.Pure
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

instance Event String

external' :: Managed (View SomeEvent, Controller SomeEvent)
external' = do
  c <- stdinLines
  return (contramap show stdoutLines,fmap SomeEvent c)

data TestAppService a = TestAppService
  { _testCount :: Int
  }

newTestAppService :: AppStateAPI (TestAppService a) -> SomeAppService SomeEvent SomeEvent a
newTestAppService api = SomeAppService 0 api Just id (TestAppService 0)

instance AppService (TestAppService a) where
  type AppState (TestAppService a) = a
  type EventIn (TestAppService a) = SomeEvent
  type EventOut (TestAppService a) = SomeEvent
  data AppStateAPI (TestAppService a) = TestAppServiceAPI
    { _testQuery :: a -> Int
    , _testModify :: a -> a
    }
  processEvent _ e
    | Just ("inc"::String) <- fromEvent e = do
        api <- getAppStateAPI
        v <- getsAppState (_testQuery api)
        modifyAppState (_testModify api)
        v' <- getsAppState (_testQuery api)
        return
          [ releaseEvent . Msg $ "state pre inc: " ++ show v
          , releaseEvent . Msg $ "state post inc: " ++ show v'
          ]
    | Just ("id"::String) <- fromEvent e = do
        i <- getAppServiceId
        return [releaseEvent . Msg $ "App service id: " ++ show i]
    | otherwise = noEvents

data Msg = Msg String deriving (Typeable,Show)

instance Event Msg

data LogAppService a = LogAppService

instance AppService (LogAppService a) where
  type AppState (LogAppService a) = a
  type EventIn (LogAppService a) = SomeEvent
  type EventOut (LogAppService a) = SomeEvent
  data AppStateAPI (LogAppService a) = LogAppServiceAPI
  processEvent _ e = return [releaseEvent . Msg $ show e]

newLogAppService :: SomeAppService SomeEvent SomeEvent a
newLogAppService = SomeAppService 0 LogAppServiceAPI Just id LogAppService

appServices :: [SomeAppService SomeEvent SomeEvent Int]
appServices = initialiseAppServices
  [ newLogAppService
  , newTestAppService (TestAppServiceAPI id (+1))
  ]

model' :: Model Int SomeEvent SomeEvent
model' = asPipe (P.takeWhile (not . done)) >>> asPipe (runAppModel appServices handleEvent)
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

eventOut :: Either SomeEvent SomeEvent -> Either String Msg
eventOut (Left x) = Left (fromJust $ fromEvent x)
eventOut (Right x) = Right (fromJust $ fromEvent x)

newTestAppService' :: AppStateAPI (TestAppService a) -> SomeAppService String Msg a
newTestAppService' api = SomeAppService 0 api (Just . SomeEvent) eventOut (TestAppService 0)

newLogAppService' :: SomeAppService String Msg a
newLogAppService' = SomeAppService 0 LogAppServiceAPI (Just . SomeEvent) eventOut LogAppService

appServices' :: [SomeAppService String Msg Int]
appServices' = initialiseAppServices
  [ newLogAppService'
  , newTestAppService' (TestAppServiceAPI id (+1))
  ]

model'' :: Model Int String Msg
model'' = asPipe (P.takeWhile (not . done)) >>> asPipe (runAppModel appServices' handleEvent)
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


