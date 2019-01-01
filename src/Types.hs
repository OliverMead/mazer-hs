module Types where

import Data.List

type MazeLine = [Int]
type MazeData = [MazeLine]

type Position = (Int, Int)

data Node = 
    Node {  position :: Position,
            up :: Maybe Position,
            down :: Maybe Position,
            left :: Maybe Position,
            right :: Maybe Position 
         } deriving (Show, Eq)
