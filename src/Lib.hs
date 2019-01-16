module Lib where

import Control.Monad
import Data.List
import Data.Bool.HT

import Generate
import Types
import Solve
import Utils

mazeData :: MazeData
mazeData = [ [ 09,13,13 ],
             [ 06,00,08 ],
             [ 07,10,06 ] ]

mazeSize = ((length $ mazeData !! 0), length mazeData) :: MazeSize


run :: [String] -> IO ()
run args = test

test :: IO ()
test = case makeNodes (mazeData,mazeSize) of
         Left msg -> putStrLn msg
         Right nodes -> printList nodes
                        >> printMaze nodes mazeSize
                        >> printList (solve nodes mazeSize)
                        >> solvePrint nodes mazeSize

solvePrint :: [Node] -> MazeSize -> IO ()
solvePrint nodes size = printSolved nodes (solve nodes size) size

solve :: [Node] -> MazeSize -> [Node]
solve nodes size = removeDeadPaths nodes size (entryIndex nodes size)
