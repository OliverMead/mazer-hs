module Generate where

import Control.Monad
import Data.List
import Data.Bool.HT
import System.Random

import Types

mazeData :: MazeData
mazeData = [ [ 09,13,13 ],
             [ 06,00,08 ],
             [ 07,10,02 ] ]

mazeSize = ((length $ mazeData !! 0), length mazeData) :: MazeSize

wallsToPaths :: [Direction] -> [Direction]
wallsToPaths dirs = removeMatching dirs possible
    where 
        possible = [DUp, DDown, DLeft, DRight]
        removeMatching :: [Direction] -> [Direction] -> [Direction]
        removeMatching _ [] = []
        removeMatching notThese (dir:dirs) = if dir `elem` notThese
                                                then removeMatching notThese dirs
                                                else dir : removeMatching notThese dirs

makeNodes :: MazeMap -> Either String [Node]
makeNodes (_,(0,0)) = Left "Empty MazeMap Provided"
makeNodes ([],_) = Left "Empty MazeMap Provided"
makeNodes ([[]],_) = Left "Empty MazeMap Provided"
makeNodes (rows,(nc,nr)) = generateFrom 0
    where 
        generateFrom :: Int -> Either String [Node]
        generateFrom rowNum = case row (rowNum,0) of
                                Left msg -> Left msg
                                Right nodes -> case generateFrom $ rowNum+1 of
                                                 Left msg -> Right nodes
                                                 Right nodes' -> Right $ nodes ++ nodes'

        row :: Position -> Either String [Node]
        row (rn,cn)
          | rn >= nr || rn < 0 = Left "column out of range"
          | cn >= nc || cn < 0 = Left "row out of range"
          | otherwise = case row (rn, cn+1) of 
                          Left msg -> Right $ [makeNode (rn,cn)]
                          Right nodes -> Right $ makeNode (rn,cn) : nodes
        
        makeNode :: Position -> Node
        makeNode (i,j) = Node (j,i) up' down' left' right'
            where 
                cell = rows !! i !! j
                paths = wallsToPaths $ walls cell
                up' 
                  | not (DUp `elem` paths)   = Nothing
                  | i-1 < 0                  = Just (-1,-1) -- this is an opening
                  | otherwise                = Just (j,i-1)

                down' 
                  | not (DDown `elem` paths) = Nothing
                  | i+1 >= nr                = Just (-1,-1) -- this is an opening
                  | otherwise                = Just (j,i+1)

                left'
                  | not (DLeft `elem` paths) = Nothing
                  | j-1 < 0                  = Just (-1,-1) -- this is an opening
                  | otherwise                = Just (j-1,i)

                right' 
                  | not (DRight `elem` paths)= Nothing
                  | j+1 >= nc                = Just (-1,-1) -- this is an opening
                  | otherwise                = Just (j+1,i)


walls :: MazeCell -> [Direction]
walls x
  | x > 15 = walls 15
  | x >= 8 = DRight : (walls $ x - 8)
  | x >= 4 = DLeft : (walls $ x - 4)
  | x >= 2 = DDown : (walls $ x - 2)
  | x >= 1 = DUp : (walls $ x - 1)
  | otherwise = []

randomMaze :: MazeSize -> MazeMap
randomMaze (x,y) = (layout, size) 
    where
        layout = mazeData
        size = mazeSize

genMaze' :: MazeMap -> Either String [Node]
genMaze' (_,(0,0)) = Left "Empty MazeData Provided"
genMaze' ([],_) = Left "Empty MazeData Provided"
genMaze' (theList,(x,y)) = generateFrom 0
    where
        generateFrom :: Int -> Either String [Node]
        generateFrom !pos = case nodeLine (pos,0) of
                              Left msg -> Left msg
                              Right nodes -> case generateFrom $ pos+1 of
                                               Left msg -> Right nodes
                                               Right nodes' -> Right $ nodes ++ nodes'

        nodeLine :: Position -> Either String [Node]
        nodeLine (i,j) 
          | i >= y || i < 0 = Left "column out of range"
          | j >= x || j < 0 = Left "row out of range"
          | otherwise = case makeNode (i, j) of
                          Left msg -> case nodeLine (i,j+1) of
                                        Left msg -> Right []
                                        Right nodes -> Right nodes
                          Right node -> case nodeLine (i, j+1) of
                                          Left msg -> Right [node]
                                          Right nodes -> Right $ node : nodes

        makeNode :: Position -> Either String Node
        makeNode (i,j) = case theList !! i !! j of
                         1 -> Left $ "Wall at (" ++ show j ++ "," ++ show i ++ ")"
                         0 -> Right $ Node (j,i) up' down' left' right'
            where 
                up' 
                  | i-1 < 0 = Nothing 
                  | theList !! (i-1) !! j == 1 = Nothing
                  | otherwise = Just (j,i-1)

                down' 
                  | i+1 >= y = Nothing
                  | theList !! (i+1) !! j == 1 = Nothing
                  | otherwise = Just (j,i+1)

                left'
                  | j-1 < 0 = Nothing 
                  | theList !! i !! (j-1) == 1 = Nothing
                  | otherwise = Just (j-1,i)

                right' 
                  | j+1 >= x = Nothing
                  | theList !! i !! (j+1) == 1 = Nothing
                  | otherwise = Just (j+1,i)
