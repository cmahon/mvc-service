{-# LANGUAGE ExistentialQuantification #-}

module MVC.Event where

import Data.Typeable

-----------------------------------------------------------------------------

class (Typeable a, Show a) => Event a

data SomeEvent = forall a. (Event a) => SomeEvent a

type EitherSomeEvent = Either SomeEvent SomeEvent

instance Show SomeEvent where show (SomeEvent e) = show e

fromEvent :: Event e => SomeEvent -> Maybe e
fromEvent (SomeEvent e) = cast e

data Match m = forall a. (Event a, Monad m) => Match (a -> m ())

handleEvent :: [Match m] -> (SomeEvent -> m ()) -> SomeEvent -> m ()
handleEvent matches nomatch e = foldr check' (nomatch e) matches
  where
  check' (Match f) ms = case fromEvent e of 
    Just e' -> f e'
    Nothing -> ms




