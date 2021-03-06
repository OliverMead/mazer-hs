module Lib where

import           Control.Monad
import           Data.List
-- import           Data.Bool

import           Generate -- src/Generate.hs
import           Types -- src/Types.hs
import           Solve -- src/Solve.hs
import           Utils -- src/Utils.hs

mazeData :: MazeData
mazeData = [[09, 13, 13], [06, 00, 08], [07, 10, 06]]

mazeSize = ((length $ mazeData !! 0), length mazeData) :: MazeSize


run :: [String] -> IO ()
run args = test

test :: IO ()
test = case makeNodes (mazeData, mazeSize) of
    Left msg -> putStrLn msg
    -- Right nodes -> printList nodes
    Right nodes ->
        printMaze nodes mazeSize
                   -- >> printList (solve nodes mazeSize)
                                 >> solvePrint nodes mazeSize

solvePrint :: Nodes -> MazeSize -> IO ()
solvePrint nodes size = printSolved nodes (solve nodes size) size

solve :: Nodes -> MazeSize -> Nodes
solve nodes size = removeDeadPaths nodes size (entryIndex nodes size)
