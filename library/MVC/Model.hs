{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module MVC.Model where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader             (MonadReader (..))
import           Control.Monad.State              (MonadState (..))
import           Control.Monad.Trans.Reader       (ReaderT)
import qualified Control.Monad.Trans.Reader       as R
import           Control.Monad.Trans.State.Strict (State, StateT)
import qualified Control.Monad.Trans.State.Strict as S
import           Data.Monoid
import           Data.Traversable                 (traverse)
import           Lens.Family                      (LensLike',set)
import           Lens.Family.State.Strict         (zoom)
import           Pipes

import           MVC.Event

-------------------------------------------------------------------------------

-- Note: superseded by EventHandler, will be retired when dependencies migrated

-------------------------------------------------------------------------------

type AppPipe a = 
  Pipe SomeEvent SomeEvent (State a) 

newtype AppModel a r = 
  AppModel (StateT [SomeAppService a] (AppPipe a) r) 
  deriving (Functor,Applicative,Monad,MonadState [SomeAppService a])

newtype AppModel' a r = 
  AppModel' (StateT (SomeAppService a) (AppPipe a) r) 
  deriving (Functor,Applicative,Monad,MonadState (SomeAppService a))

newtype AppServiceModel a r = 
  AppServiceModel (ReaderT (Int,AppStateAPI a) (StateT a (State (AppState a))) r) 
  deriving (Functor,Applicative,Monad,MonadReader (Int,AppStateAPI a))

type AppServiceModelEvents a = 
  AppServiceModel a [EitherSomeEvent]

class AppService a where
  type AppState a :: *
  data AppStateAPI a :: *
  processEvent :: a -> SomeEvent -> AppServiceModelEvents a

data SomeAppService :: * -> * where
  SomeAppService :: (AppService a, AppState a ~ b) => 
    { _asId :: Int
    , _asAPI :: AppStateAPI a
    , _asAppService :: a
    } -> SomeAppService b

asId :: Functor f => LensLike' f (SomeAppService a) Int
asId f (SomeAppService i a s) = (\i' -> SomeAppService i' a s) <$> f i

-----------------------------------------------------------------------------

initialiseAppServices :: [SomeAppService a] -> [SomeAppService a]
initialiseAppServices appservices = zipWith (set asId) [1..] appservices 

forAppServices :: (Monoid r) => AppModel' a r -> AppModel a r
forAppServices (AppModel' am') = AppModel $ zoom traverse am'

releaseEvent :: SomeEvent -> AppModel a ()
releaseEvent = AppModel . lift . yield

getAppServiceM :: AppModel' a (SomeAppService a)
getAppServiceM = AppModel' S.get

putAppServiceM :: SomeAppService a -> AppModel' a ()
putAppServiceM = AppModel' . S.put

getAppStateM :: AppModel' a a
getAppStateM = AppModel' $ lift $ lift S.get

putAppStateM :: a -> AppModel' a ()
putAppStateM = AppModel' . lift . lift . S.put

runAppModel :: [SomeAppService a] -> AppModel a r -> AppPipe a r
runAppModel appservices (AppModel appmodel) = S.evalStateT appmodel appservices 

runAppServiceModel :: SomeAppService a -> a -> SomeEvent -> ([EitherSomeEvent],SomeAppService a,a)
runAppServiceModel SomeAppService{..} appstate event = 
  let
    (AppServiceModel appServiceModel) = processEvent _asAppService event
    ((events,appservice'),appstate') = S.runState (S.runStateT (R.runReaderT appServiceModel (_asId,_asAPI)) _asAppService) appstate
  in 
    (events,(SomeAppService _asId _asAPI appservice'),appstate')

getAppServiceId :: AppServiceModel a Int
getAppServiceId = AppServiceModel (R.asks fst)

getAppStateAPI :: AppServiceModel a (AppStateAPI a)
getAppStateAPI = AppServiceModel (R.asks snd)

getAppService :: AppService a => AppServiceModel a a
getAppService = AppServiceModel $ lift $ S.get

putAppService :: AppService a => a -> AppServiceModel a ()
putAppService = AppServiceModel . lift . S.put 

getAppState :: AppServiceModel a (AppState a)
getAppState = AppServiceModel $ lift $ lift $ S.get

putAppState :: AppState a -> AppServiceModel a ()
putAppState = AppServiceModel . lift . lift . S.put

getsAppState :: (AppState a -> r) -> AppServiceModel a r
getsAppState f = liftM f getAppState 

modifyAppState :: (AppState a -> AppState a) -> AppServiceModel a ()
modifyAppState = AppServiceModel . lift . lift . S.modify

modifyAppState' :: (AppState a -> (r,AppState a)) -> AppServiceModel a r
modifyAppState' f = do
  s <- getAppState
  let (r,s') = f s
  putAppState s'
  return r

modifyAppState'' :: (AppState a -> Maybe (r,AppState a)) -> AppServiceModel a (Maybe r)
modifyAppState'' f = do
  s <- getAppState
  maybe (return Nothing) (\(r,s') -> putAppState s' >> return (Just r)) (f s)

withAppState :: (AppState a -> AppServiceModel a r) -> AppServiceModel a r
withAppState f = getAppState >>= f

noEvents :: AppServiceModel a [b] 
noEvents = return []

propagate :: Event e => e -> EitherSomeEvent
propagate = Left . SomeEvent

release :: Event e => e -> EitherSomeEvent
release = Right . SomeEvent

handleEvent :: AppModel a ()
handleEvent = forever $ AppModel (lift await) >>= go
  where 
  go e = do
    r <- forAppServices $ do
      appSvc <- getAppServiceM
      appState <- getAppStateM 
      let (r,appSvc',appState') = runAppServiceModel appSvc appState e  
      putAppServiceM appSvc'
      putAppStateM appState'
      return r
    mapM_ (either go releaseEvent) r


