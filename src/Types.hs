module Types where

import Data.List

{-  
    MazeCell:
    1 = top wall
    2 = bottom wall
    4 = left wall
    8 = right wall
    other values refer to multiple walls 
    (max number 15 for this, or 14 for usable numbers)
    e.g.
        0 = no walls
        3 = top and bottom
        5 = top and left wall
        6 = bottom and left wall
        7 = all but right walls
        9 = top and right walls
        10 = right and bottom walls
        15 = all walls
        12 = left and right walls
        14 = bottom, left and right walls
-}

type MazeCell = Int
type MazeLine = [MazeCell]
type MazeData = [MazeLine]

type Position = (Int, Int)
type MazeSize = Position

type MazeMap = (MazeData,MazeSize)

data Direction = DUp | DDown | DLeft | DRight deriving (Eq)

data Node = 
    Node {  position :: Position,
            up :: Maybe Position,
            down :: Maybe Position,
            left :: Maybe Position,
            right :: Maybe Position 
         } deriving (Show, Eq)
