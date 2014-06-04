{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module MVC.Model.ListT where

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

--import           MVC.Model.Pure                   hiding (AppServiceModel(..),AppServiceModelEvents(..))

-----------------------------------------------------------------------------

--Work in progress, pending refactor

--type AppPipe b c a = Pipe b c (State a) 

--newtype AppModel b c a r = 
--  AppModel (StateT [SomeAppService b c a] (AppPipe b c a) r) 
--  deriving (Functor,Applicative,Monad,MonadState [SomeAppService b c a])

--newtype AppModel' b c a r = 
--  AppModel' (StateT (SomeAppService b c a) (AppPipe b c a) r) 
--  deriving (Functor,Applicative,Monad,MonadState (SomeAppService b c a))

type AppServiceModel a r = 
  ListT (ReaderT (Int,AppStateAPI a) (StateT a (State (AppState a)))) r
  --deriving (Functor,Applicative,Monad,MonadReader (Int,AppStateAPI a))

type AppServiceModelEvents b c a = 
  AppServiceModel a (Either b c)

--class AppService a where
--  type AppState a :: *
--  type EventIn a :: *
--  type EventOut a :: *
--  data AppStateAPI a :: *
--  processEvent :: a -> EventIn a -> AppServiceModelEvents (EventIn a) (EventOut a) a

--data SomeAppService :: * -> * -> * -> * where
--  SomeAppService :: (AppService s, AppState s ~ a, EventIn s ~ b, EventOut s ~ c) => 
--    { _asId :: Int
--    , _asAPI :: AppStateAPI s
--    , _asEventIn :: b' -> Maybe b
--    , _asEventOut :: Either b c -> Either b' c'
--    , _asAppService :: s
--    } -> SomeAppService b' c' a

--asId :: Functor f => LensLike' f (SomeAppService b c a) Int
--asId f (SomeAppService i a ein eout s) = (\i' -> SomeAppService i' a ein eout s) <$> f i

-----------------------------------------------------------------------------

--initialiseAppServices :: [SomeAppService b c a] -> [SomeAppService b c a]
--initialiseAppServices appservices = zipWith (set asId) [1..] appservices 

--forAppServices :: (Monoid r) => AppModel' b c a r -> AppModel b c a r
--forAppServices (AppModel' am') = AppModel $ zoom traverse am'

--releaseM :: c -> AppModel b c a ()
--releaseM = AppModel . lift . yield

--getAppServiceM :: AppModel' b c a (SomeAppService b c a)
--getAppServiceM = AppModel' S.get

--putAppServiceM :: SomeAppService b c a -> AppModel' b c a ()
--putAppServiceM = AppModel' . S.put

--getAppStateM :: AppModel' b c a a
--getAppStateM = AppModel' $ lift $ lift S.get

--putAppStateM :: a -> AppModel' b c a ()
--putAppStateM = AppModel' . lift . lift . S.put

--runAppModel :: [SomeAppService b c a] -> AppModel b c a r -> AppPipe b c a r
--runAppModel appservices (AppModel appmodel) = S.evalStateT appmodel appservices 

runAppServiceModel :: SomeAppService b c a -> a -> b -> ([Either b c],SomeAppService b c a,a)
runAppServiceModel appservice@SomeAppService{..} appstate event = 
  maybe ignore process (_asEventIn event)
  where 
  ignore = ([],appservice,appstate)
  process event' = 
    let
      (AppServiceModel appServiceModel) = processEvent _asAppService event'
      ((events,appservice'),appstate') = S.runState (S.runStateT (R.runReaderT appServiceModel (_asId,_asAPI)) _asAppService) appstate
    in 
      (map _asEventOut events,(SomeAppService _asId _asAPI _asEventIn _asEventOut appservice'),appstate')

getAppServiceId :: AppServiceModel a Int
getAppServiceId = Select (R.asks fst)

getAppStateAPI :: AppServiceModel a (AppStateAPI a)
getAppStateAPI = Select (R.asks snd)

getAppService :: AppService a => AppServiceModel a a
getAppService = Select $ lift $ S.get

putAppService :: AppService a => a -> AppServiceModel a ()
putAppService = Select . lift . S.put 

getAppState :: AppServiceModel a (AppState a)
getAppState = Select $ lift $ lift $ S.get

putAppState :: AppState a -> AppServiceModel a ()
putAppState = Select . lift . lift . S.put

--getsAppState :: (AppState a -> r) -> AppServiceModel a r
--getsAppState f = liftM f getAppState 

modifyAppState :: (AppState a -> AppState a) -> AppServiceModel a ()
modifyAppState = Select . lift . lift . S.modify

--modifyAppState' :: (AppState a -> (r,AppState a)) -> AppServiceModel a r
--modifyAppState' f = do
--  s <- getAppState
--  let (r,s') = f s
--  putAppState s'
--  return r

--modifyAppState'' :: (AppState a -> Maybe (r,AppState a)) -> AppServiceModel a (Maybe r)
--modifyAppState'' f = do
--  s <- getAppState
--  maybe (return Nothing) (\(r,s') -> putAppState s' >> return (Just r)) (f s)

--withAppState :: (AppState a -> AppServiceModel a r) -> AppServiceModel a r
--withAppState f = getAppState >>= f

noEvents :: AppServiceModel a [b] 
noEvents = mzero

propagate :: b -> Either b c
propagate = yield . Left 

propagateEvent :: Event b => b -> EitherSomeEvent
propagateEvent = propagate . SomeEvent 

release :: c -> Either b c
release = yield . Right

releaseEvent :: Event c => c -> EitherSomeEvent
releaseEvent = release . SomeEvent 

handleEvent :: AppModel b c a ()
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
    mapM_ (either go releaseM) r

--handleEventL :: SomeEvent -> AppModelL a
--handleEventL = go
--  where
--  go e = do
--    s <- IntMap.elems <$> use appServices
--    l <- mconcat $ (map processEventL' s) <*> [e]
--    either go return l 

-----------------------------------------------------------------------------

--type AppModelL a = ListT (State (AppStateL a)) SomeEvent
--type AppModelL' a = ListT (State (AppStateL a)) (SomeEvent')
--type AppModelL'' a = Producer (SomeEvent') (State (AppStateL a)) ()

--broadcast :: Event e => e -> AppModelL'' a
--broadcast = yield . Left . SomeEvent

--issue :: Event e => e -> AppModelL'' a
--issue = yield . Right . SomeEvent

--class BaseServiceL a where
--  type ADBL a :: *
--  getServiceIdL :: a -> Int
--  setServiceIdL :: a -> Int -> a
--  processEventL :: a -> SomeEvent -> AppModelL' (ADBL a)  

--data AppServiceL b = forall a. (BaseServiceL a, ADBL a ~ b) => AppServiceL a

--data AppStateL a = AppStateL
--  { _appServices :: IntMap (AppServiceL a)
--  , _appDBL :: a
--  }

--makeLenses ''AppStateL

--getServiceIdL' (AppServiceL x) = getServiceIdL x
--setServiceIdL' (AppServiceL x) i = AppServiceL $ setServiceIdL x i
--processEventL' (AppServiceL x) = processEventL x

--handleEventL :: SomeEvent -> AppModelL a
--handleEventL = go
--  where
--  go e = do
--    s <- IntMap.elems <$> use appServices
--    l <- mconcat $ (map processEventL' s) <*> [e]
--    either go return l 

--updateAppServiceL :: BaseServiceL a => a -> AppModelL' (ADBL a) 
--updateAppServiceL s = do
--  (appServices . (at (getServiceIdL s))) .= Just (AppServiceL s)
--  mzero

--updateAppServiceL' :: BaseServiceL a => a -> Producer (SomeEvent') (State (AppStateL (ADBL a))) ()
--updateAppServiceL' s = lift $ (appServices . (at (getServiceIdL s))) .= Just (AppServiceL s)

--runAppL :: [ManagedService SomeEvent SomeEvent] -> AppStateL a -> IO ()
--runAppL services state = with (_managedService $ mconcat services) $ 
--  \(Service v c) -> runMVC c (fromListT handleEventL) v state