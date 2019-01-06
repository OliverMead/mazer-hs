module Lib (run, showMaze) where

import Control.Monad
import Data.List
import Data.Bool.HT

import Generate
import Types
import Solve
import Utils

run :: [String] -> IO ()
run args = test

test :: IO ()
test = case genMaze mazeData of
         Left msg -> putStrLn msg
         Right nodes -> showMaze nodes mazeSize
                        >> printList (removeDeadPaths nodes mazeSize)
                        >> showSolved nodes (removeDeadPaths nodes mazeSize) mazeSize

test' :: IO ()
test' = case genMaze mazeData of
         Left msg -> putStrLn msg
         Right nodes -> showMaze nodes mazeSize

printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x:xs) = print x >> printList xs

showMaze :: [Node] -> MazeSize -> IO ()
showMaze nodes (x,y) = showMazeFrom (0,0)
    where
        showMazeFrom :: Position -> IO ()
        showMazeFrom (x',y') 
          | x' > x = putStrLn "" >> showMazeFrom (0,y'+1) 
          | y' > y = return ()
          | otherwise = case nodeat (x',y') nodes of
                          Left msg -> putStr "█" >> showMazeFrom (x'+1,y')
                          Right node -> putStr " " >> showMazeFrom (x'+1, y')


