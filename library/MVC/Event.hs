{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

module MVC.Event where

import Control.Arrow ((+++))
import Data.Maybe    (fromJust)         

import Data.Typeable

-----------------------------------------------------------------------------

class (Typeable a, Show a) => Event a

data SomeEvent = forall a. (Event a) => SomeEvent a

type EitherSomeEvent = Either SomeEvent SomeEvent

instance Show SomeEvent where show (SomeEvent x) = show x

fromEvent :: Event a => SomeEvent -> Maybe a
fromEvent (SomeEvent x) = cast x

fromEvent' :: Event a => SomeEvent -> a
fromEvent' = fromJust . fromEvent

data Match m = forall a. (Event a, Monad m) => Match (a -> m ())

handleEvent :: [Match m] -> (SomeEvent -> m ()) -> SomeEvent -> m ()
handleEvent matches nomatch e = foldr check' (nomatch e) matches
  where
  check' (Match f) ms = case fromEvent e of 
    Just e' -> f e'
    Nothing -> ms

fromEitherSomeEvent :: (Event a, Event b) => Either SomeEvent SomeEvent -> Either a b
fromEitherSomeEvent = (fromJust . fromEvent) +++ (fromJust . fromEvent)

toEitherSomeEvent :: (Event a, Event b) => Either a b -> Either SomeEvent SomeEvent
toEitherSomeEvent = SomeEvent +++ SomeEvent

data Msg = Msg String deriving (Typeable)

instance Show Msg where show (Msg x) = show x

instance Event Msg




