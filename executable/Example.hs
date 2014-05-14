module Main where

import           Control.Category
import           Control.Monad           
import           Data.Monoid                      
import           MVC
import           MVC.Prelude
import           MVC.Service
import qualified Pipes.Prelude                    as P
import           Prelude                          hiding (id,(.))

-----------------------------------------------------------------------------

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

main :: IO ()
main = runMVC () model external