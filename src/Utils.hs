module Utils where

import Control.Monad
import Data.List
import Data.Bool.HT

import Types

nodeat :: Position -> [Node] -> Either String Node
-- finds first node in set of nodes at given position
nodeat pos [] = Left ("No node at " ++ show pos)
nodeat pos (x:xs) 
  | position x == pos = Right x
  | otherwise = nodeat pos xs

showCell :: MazeCell -> String
showCell n 
  | c  0 = "╬"
  | c  1 = "╦"
  | c  2 = "╩"
  | c  3 = "═"
  | c  4 = "╠"
  | c  5 = "╔"
  | c  6 = "╚"
  | c  7 = "╘"
  | c  8 = "╣"
  | c  9 = "╗"
  | c 10 = "╝"
  | c 11 = "╛"
  | c 12 = "║"
  | c 13 = "╓"
  | c 14 = "╙"
  | otherwise = "░"
  where c i = n == i

showBlocked :: MazeCell -> String
showBlocked n
  | c  0 = "┼"
  | c  1 = "┬"
  | c  2 = "┴"
  | c  3 = "─"
  | c  4 = "├"
  | c  5 = "┌"
  | c  6 = "└"
  | c  7 = "└"
  | c  8 = "┤"
  | c  9 = "┐"
  | c 10 = "┘"
  | c 11 = "┘"
  | c 12 = "│"
  | c 13 = "┌"
  | c 14 = "└"
  | otherwise = "░"
  where c i = n == i
 
showSolved :: [Node] -> [Node] -> MazeSize -> IO ()
showSolved nodes solution (x,y) = showMazeFrom (0,0)
    where
        showMazeFrom :: Position -> IO ()
        showMazeFrom (x',y') 
          | x' >= x = putStrLn "" >> showMazeFrom (0,y'+1) 
          | y' >= y = return ()
          | otherwise = case nodeat (x',y') nodes of
                          Left msg -> putStr "█" >> showMazeFrom (x'+1,y')
                          Right node -> case nodeat (x',y') solution of
                                          Left msg -> putStr "#" >> showMazeFrom (x'+1, y')
                                          Right node -> putStr " " >> showMazeFrom (x'+1,y')


entryIndex :: [Node] -> MazeSize -> (Int, Direction)
entryIndex nodes siz = entryIndexFinder nodes siz 0
    where entryIndexFinder :: [Node] -> MazeSize -> Int -> (Int,Direction)
          entryIndexFinder [] _ _ = (0,DUp)
          entryIndexFinder ((Node (x',y') _ _ _ _):ns) (x,y) i 
            =   if x==x' && isOpen then (i,DRight)
                   else if x'==0 && isOpen then (i,DLeft)
                           else if y==y' && isOpen then (i,DDown)
                                   else if y'==0 && isOpen then (i,DUp)
                                        else entryIndexFinder ns (x,y) (i+1)
            where isOpen = (-1,-1) == (x', y')
