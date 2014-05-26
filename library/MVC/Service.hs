{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MVC.Service where

import           Control.Applicative
import           Control.Category
import           Control.Concurrent.Async
import           Control.Monad           
import           Data.Profunctor
import           Data.Monoid                      
import           MVC
import           MVC.Event
import           Prelude                          hiding (id,(.))

-----------------------------------------------------------------------------

data ServiceCommand =
    ServiceStart 
  | ServicePause 
  | ServiceResume  
  | ServiceStop
  | ServiceReportStatus
  deriving (Eq,Ord,Show)

data ServiceStatus =
    ServicePending
  | ServiceActivating
  | ServiceActive
  | ServiceTerminating
  | ServiceTerminated
    deriving (Eq,Ord,Show)

data ConnectionStatus = 
    ServiceConnecting
  | ServiceConnected
  | ServiceDisconnecting
  | ServiceDisconnected
    deriving (Eq,Ord,Show)

-----------------------------------------------------------------------------

data Service a b = Service 
  { request :: Output a
  , response :: Input b
  }

instance Profunctor Service where
  lmap f (Service req resp) = Service (Output (send req . f)) resp
  rmap f (Service req resp) = Service req (fmap f resp)

instance Monoid (Service a b) where
  mempty = Service mempty mempty
  mappend s1 s2 = Service (request s1 <> request s2) (response s1 <> response s2) 

newtype ManagedService a b = ManagedService
  { _fromManagedService :: Managed (Service a b)
  } deriving (Monoid) 

toManagedService :: Managed (Service a b) -> ManagedService a b
toManagedService = ManagedService

instance Profunctor ManagedService where
  lmap f (ManagedService ms) = ManagedService $ fmap (lmap f) ms
  rmap f (ManagedService ms) = ManagedService $ fmap (rmap f) ms

instance Category ManagedService where
  id = ManagedService $ managed $ \k -> do 
    (output, input, seal) <- spawn' Unbounded
    k (Service output input) <* atomically seal
  (ManagedService ms2) . (ManagedService ms1) = ManagedService $ do
    (Service req2 resp2) <- ms2
    (Service req1 resp1) <- ms1
    managed $ \k -> do
      let io = runEffect (fromInput resp1 >-> toOutput req2) <* performGC
      withAsync io $ \a -> k (Service req1 resp2) <* performGC <* wait a     

fromPipe :: Buffer a -> Buffer b -> Pipe a b IO () -> Managed (Service a b)
fromPipe reqBuffer respBuffer pipe =
  managed $ \k -> do
    (reqOutput, reqInput, _) <- spawn' reqBuffer
    (respOutput, respInput, _) <- spawn' respBuffer
    let io = runEffect (fromInput reqInput >-> pipe >-> toOutput respOutput) <* performGC 
    withAsync io $ \a -> k (Service reqOutput respInput) <* performGC <* wait a

toManagedMVC :: ManagedService a b -> Managed (View a, Controller b)
toManagedMVC ms = do
  (Service req resp) <- _fromManagedService ms
  return (asSink (void . atomically . send req), asInput resp)

handlesOutput :: (a -> Maybe b) -> Output b -> Output a
handlesOutput f o = Output $ \a -> case f a of
  Nothing -> return True
  Just b  -> send o b

accepts ::  (a -> Maybe b) -> ManagedService b c -> ManagedService a c
accepts f (ManagedService ms) = ManagedService $ fmap f' ms
  where
  f' (Service req resp) = Service (handlesOutput f req) resp 

acceptsEvent :: Event b => ManagedService b c -> ManagedService SomeEvent c
acceptsEvent = accepts fromEvent 

generatesEvent :: Event b => ManagedService a b -> ManagedService a SomeEvent
generatesEvent = rmap SomeEvent 

processesEvent :: (Event b, Event c) => ManagedService b c -> ManagedService SomeEvent SomeEvent
processesEvent = acceptsEvent . generatesEvent
