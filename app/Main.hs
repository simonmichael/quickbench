{-
quickbench - simple benchmarking of command-line programs.
(c) Simon Michael 2008-2016
-}

module Main where

import System.Exit
import QuickBench (defaultMain)

main :: IO ()
main = defaultMain >>= maybe exitSuccess die
