module Main (main) where

import           Criterion.Main (bgroup, defaultMain)
import qualified MVC.ServiceBench

main :: IO ()
main = defaultMain
    [ bgroup "MVC.Service" MVC.ServiceBench.benchmarks
    ]
