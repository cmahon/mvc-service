{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module MVC.Socket where

import           Control.Applicative
import           Control.Category          ((>>>))
import           Control.Concurrent        (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens              hiding (view)
import           Control.Monad             hiding (forM_)
import           Data.ByteString           (ByteString)
import           Data.Default
import           Data.Foldable             (forM_)
import           MVC
import           MVC.Service
import           Network.Simple.TCP        hiding (send)
import qualified Network.Simple.TCP        as S (send)
import           Pipes.Network.TCP         (fromSocket)
import qualified System.Log.Logger         as L
import           System.Log.Logger.TH      (deriveLoggers)

-----------------------------------------------------------------------------

$(deriveLoggers "L" [L.DEBUG])

-----------------------------------------------------------------------------

data ConnectionCommand = 
    Connect Int
  | Disconnect
    deriving (Eq,Show)

data ServiceIn = 
    ServiceCommand ServiceCommand
  | ConnectionCommand ConnectionCommand  
  | Send ByteString
    deriving (Eq,Show)

data EventIn = 
    ServiceIn ServiceIn
  | SocketIn ByteString
  | ConnectionIn Bool
    deriving (Eq,Show)

data ServiceOut = 
    ServiceStatus ServiceStatus
  | Stream ByteString
  | Log ByteString
    deriving (Eq,Show)

data EventOut =
    ServiceOut ServiceOut
  | SocketOut ByteString
  | ConnectionOut ConnectionCommand
  | Done
    deriving (Eq,Show)

makePrisms ''EventIn
makePrisms ''EventOut

type SocketService = Service ServiceIn ServiceOut

-----------------------------------------------------------------------------

data SocketParams = SocketParams
  { _spHostName :: HostName
  , _spServiceName :: ServiceName
  } deriving (Eq,Show)

instance Default SocketParams where
  def = SocketParams "" ""

data SocketConfiguration = SocketConfiguration
  { _scAutoStart :: Bool
  , _scSocketParams :: SocketParams
  } deriving (Eq,Show)

instance Default SocketConfiguration where
  def = SocketConfiguration False def

data SocketState = SocketState
  { _ssConfig :: SocketConfiguration
  , _ssServiceStatus :: ServiceStatus
  , _ssConnectionStatus :: ConnectionStatus
  } deriving (Eq,Show)

instance Default SocketState where
  def = SocketState def ServicePending ServiceDisconnected

newtype Connection = Connection 
  { _unConnection :: TVar (Maybe Socket) }

makeLenses ''SocketParams
makeLenses ''SocketConfiguration
makeLenses ''SocketState

-----------------------------------------------------------------------------

newEmptyConnection :: IO Connection
newEmptyConnection = Connection <$> newTVarIO Nothing

newConnection :: Socket -> IO Connection
newConnection sock = Connection <$> newTVarIO (Just sock)

getSocket :: Connection -> STM (Maybe Socket)
getSocket conn = readTVar $ _unConnection conn

putSocket :: Connection -> Socket -> STM ()
putSocket conn sock = writeTVar (_unConnection conn) (Just sock) 

openConnection :: Connection -> SocketParams -> IO Bool
openConnection conn p = do 
  closeConnection conn
  r <- connectSocket p
  either (const $ return False) connection' r
  where
  connection' (sock,_) = atomically (putSocket conn sock) >> return True

closeConnection :: Connection -> IO ()
closeConnection conn = do
  sock <- atomically (getSocket conn)
  forM_ sock closeSock
  atomically $ clearSocket conn 

clearSocket :: Connection -> STM ()
clearSocket conn = writeTVar (_unConnection conn) Nothing

connection :: SocketParams -> IO (Maybe Connection)
connection p = connectSocket p >>= either (const $ return Nothing) connection'
  where
  connection' (sock,_) = Just <$> newConnection sock

connectSocket :: SocketParams -> IO (Either SomeException (Socket,SockAddr))
connectSocket SocketParams{..} = try $ connectSock _spHostName _spServiceName

-----------------------------------------------------------------------------

connectionReader :: Buffer ByteString -> Connection -> Int -> Output Bool -> Managed (Controller ByteString)
connectionReader buffer conn nbytes status = managed $ \k -> do
  
  (vOut, cOut, sOut) <- spawn' buffer
  
  let 

    sealAll :: IO ()
    sealAll = atomically sOut

    streamErrorHandler :: SomeException -> IO ()
    streamErrorHandler e =
      debugM $ "connectionReader stream error: " ++ show e
      --throwIO e

    stream :: Socket -> IO ()
    stream sock = handle streamErrorHandler $ do
      runEffect $ fromSocket sock nbytes >-> toOutput vOut
      void $ atomically $ clearSocket conn >> send status False

    ioErrorHandler :: SomeException -> IO ()
    ioErrorHandler e = do
      debugM $ "connectionReader error: " ++ show e
      sealAll
      --throwIO e

    io :: IO ()
    io = handle ioErrorHandler $ forever $ do
      sock <- atomically $ getSocket conn >>= maybe retry return
      void $ async $ stream sock
      void $ atomically $ getSocket conn >>= maybe (return ()) (const retry)

    stop :: Async () -> IO ()
    stop a = do
      debugM "connectionReader stop"
      cancel a
      sealAll

  withAsync io $ \a -> k (asInput cOut) <* stop a

-----------------------------------------------------------------------------

connectionWriter :: Connection -> Output Bool -> ByteString -> IO ()
connectionWriter conn status bs =
  atomically (getSocket conn) >>= maybe (return ()) sendSock
  where
  sendSock :: Socket -> IO ()  
  sendSock sock = trySendSock sock >>= either (const disconnected) return  
  trySendSock :: Socket -> IO (Either SomeException ())
  trySendSock sock = try $ S.send sock bs
  disconnected :: IO ()
  disconnected = void $ atomically $ clearSocket conn >> send status False

-----------------------------------------------------------------------------

connectionHandler :: SocketParams -> Connection -> Output Bool -> ConnectionCommand -> IO ()
connectionHandler p conn status cmd = case cmd of
  Connect d -> threadDelay (d * 1000000) >> openConnection conn p >>= void . atomically . send status
  Disconnect -> closeConnection conn >> void (atomically $ send status False)

-----------------------------------------------------------------------------

socketServiceModel :: Model SocketState EventIn EventOut
socketServiceModel = start >>> model >>> untilDone
  where
  
  start :: Model SocketState EventIn EventIn
  start = asPipe $ do
    autoStart <- lift $ use (ssConfig . scAutoStart)
    when autoStart $ yield $ ServiceIn $ ServiceCommand ServiceStart
    cat
  
  model :: Model SocketState EventIn EventOut
  model = asPipe $ loop $ \event -> Select $
    case event of
      (ServiceIn (ServiceCommand ServiceStart))        -> yield (ConnectionOut (Connect 0))
      (ServiceIn (ServiceCommand ServicePause))        -> yield (ConnectionOut Disconnect) 
      (ServiceIn (ServiceCommand ServiceResume))       -> yield (ConnectionOut (Connect 0))
      (ServiceIn (ServiceCommand ServiceStop))         -> yield Done
      (ServiceIn (ServiceCommand ServiceReportStatus)) -> reportServiceStatus
      (ServiceIn (ConnectionCommand c))                -> yield (ConnectionOut c)
      (ServiceIn (Send bs))                            -> yield (SocketOut bs)
      (SocketIn bs)                                    -> yield (ServiceOut (Stream bs))
      (ConnectionIn c)                                 -> updateServiceStatus c
    where
    reportServiceStatus = do
      s <- lift (use ssServiceStatus)
      yield $ ServiceOut $ ServiceStatus s
    updateServiceStatus isconnected = do
      lift $ ssServiceStatus .= toServiceStatus isconnected
      reportServiceStatus
    toServiceStatus True = ServiceActive
    toServiceStatus False = ServicePending

  untilDone :: Model SocketState EventOut EventOut
  untilDone = asPipe go
    where
    go = do
      e <- await
      case e of
        Done -> void $ do
          yield (ServiceOut $ Log "Socket Done")
          yield (ServiceOut $ ServiceStatus ServiceTerminating)
          yield (ConnectionOut Disconnect)
          yield (ServiceOut $ ServiceStatus ServiceTerminated)
        _ -> yield e >> go

-----------------------------------------------------------------------------

socketService :: SocketConfiguration -> Managed SocketService
socketService SocketConfiguration{..} = do

  conn <- managed $ \k -> newEmptyConnection >>= k

  (vConnection, cConnection, sConnection) <- managed $ \k -> spawn' Unbounded >>= k

  cSocketIn <- connectionReader Single conn 4096 vConnection

  managed $ \k -> do

    (vServiceIn , cServiceIn , sServiceIn)  <- spawn' Unbounded
    (vServiceOut, cServiceOut, sServiceOut) <- spawn' Unbounded
  
    let

      sealAll :: IO ()
      sealAll = atomically $ sServiceIn >> sServiceOut >> sConnection

      view :: View EventOut
      view = mconcat
        [ --asSink (putStrLn . ("socketService view: " ++) . show)
          handles _ServiceOut (asSink $ void . atomically . send vServiceOut)
        , handles _SocketOut (asSink $ connectionWriter conn vConnection)
        , handles _ConnectionOut (asSink $ connectionHandler _scSocketParams conn vConnection)
        ]

      controller :: Controller EventIn
      controller = mconcat
        [ ServiceIn <$> asInput cServiceIn
        , SocketIn <$> cSocketIn
        , ConnectionIn <$> asInput cConnection
        ]

      external :: Managed (View EventOut, Controller EventIn)
      external = return (view,controller)

      errorHandler :: SomeException -> IO ()
      errorHandler e = do
        debugM $ "socketService error: " ++ show e
        --TODO: take action depending on whehther async (ThreadKilled)
        --or sync exception
        closeConnection conn
        sealAll
        --throwIO e
      
      io :: IO ()
      io = do
        debugM "socketService start"
        handle errorHandler $ void $ runMVC def socketServiceModel external
        debugM "socketService end"

      stop :: Async () -> IO ()
      stop a = do
        debugM "socket service stop"
        closeConnection conn
        cancel a
        sealAll
    
    withAsync io $ \a -> k (Service vServiceIn cServiceOut) <* stop a

