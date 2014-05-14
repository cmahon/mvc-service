module MVC.ListTModel where

--Work in progress, pending refactor

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