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
                        >> printList (removeDeadPaths nodes mazeSize (entryIndex nodes mazeSize))
                        >> printSolved  
                               nodes
                               (removeDeadPaths nodes mazeSize (entryIndex nodes mazeSize)) 
                               mazeSize

printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x:xs) = print x >> printList xs

printMaze :: [Node] -> MazeSize -> IO ()
printMaze nodes (x,y) = showMazeFrom (0,0)
    where 
        showMazeFrom :: Position -> IO ()
        showMazeFrom (x',y') 
          | x' >= x = putStrLn "" >> showMazeFrom (0,y'+1)
          | y' >= y = return ()
          | otherwise = case nodeat (x',y') nodes of
                            Left msg -> putStr " " >> showMazeFrom (x'+1,y')
                            Right node -> (putStr . showCell . cell $ node) >> showMazeFrom (x'+1,y')

