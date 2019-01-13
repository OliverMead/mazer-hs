module Types where

import Data.List

type MazeLine = [Int]
type MazeData = [MazeLine]

type Position = (Int, Int)
type MazeSize = Position

type MazeMap = (MazeData,MazeSize)

data Direction = MyUp | MyDown | MyLeft | MyRight deriving (Eq)

data Node = 
    Node {  position :: Position,
            up :: Maybe Position,
            down :: Maybe Position,
            left :: Maybe Position,
            right :: Maybe Position 
         } deriving (Show, Eq)
