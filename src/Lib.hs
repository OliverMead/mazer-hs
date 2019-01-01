module Lib (run) where

import System.Directory
import System.IO
import Control.Monad
import Data.List
import Data.Bool.HT

type Pos = (Integer, Integer)

data Node = 
    Node {  position :: Pos,
            up :: Maybe Pos,
            down :: Maybe Pos,
            left :: Maybe Pos,
            right :: Maybe Pos 
         } deriving (Show, Eq)

run :: [String] -> IO ()
run args = test

test :: IO ()
test = case node of
         Left msg -> putStrLn msg
         Right node -> print node
    where node = nodeat (7,2) maze

minX = 0
minY = 0

maxX = 9
maxY = 9

maze :: [Node]
maze = [Node (x,y) 
             (y+1 <= maxY ?: (Just (x,y+1), Nothing))
             (y-1 >= minY ?: (Just (x,y-1), Nothing))
             (x-1 >= minX ?: (Just (x-1,y), Nothing))
             (x+1 <= maxX ?: (Just (x+1,y), Nothing))
               | x <- [minX..maxX], y <- [minY..maxY]]

printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x:xs) = print x >> printList xs

nodeat :: Pos -> [Node] -> Either String Node
-- finds first node in set of nodes at given position
nodeat pos [] = Left ("No node at " ++ show pos)
nodeat pos (x:xs) 
  | position x == pos = Right x
  | otherwise = nodeat pos xs
