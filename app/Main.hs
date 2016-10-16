module Main where

import System.Exit
import QuickBench (defaultMain)

main :: IO ()
main = defaultMain >>= maybe exitSuccess die
