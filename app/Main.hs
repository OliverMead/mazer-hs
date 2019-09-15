module Main where

import           System.Environment
import           Lib -- src/Lib.hs

main :: IO ()
main = getArgs >>= run
