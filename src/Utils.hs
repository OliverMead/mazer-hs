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

entryIndex :: [Node] -> MazeSize -> Int
entryIndex nodes siz = entryIndexFinder nodes siz 0
    where entryIndexFinder :: [Node] -> MazeSize -> Int -> Int
          entryIndexFinder [] _ _ = 0
          entryIndexFinder ((Node (x',y') _ _ _ _):ns) (x,y) i =  if x==x' || x==0 || y==y' || y==0
                                                                    then i
                                                                    else entryIndexFinder ns (x,y) (i+1)
