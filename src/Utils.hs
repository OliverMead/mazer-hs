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

