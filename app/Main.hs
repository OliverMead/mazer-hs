module Main where

import Lib
import System.Environment
import System.Directory
import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
    args <- getArgs
    run args
