module Generate where

import Control.Monad
import Data.List
import Data.Bool.HT

import Types

mazeData :: MazeData
mazeData = [ [ 1,1,1,1,1,1,1 ],
             [ 0,0,0,0,1,0,1 ],
             [ 1,0,1,0,0,0,1 ],
             [ 1,0,1,1,0,1,1 ],
             [ 1,1,0,0,0,1,1 ],
             [ 1,1,0,1,1,0,1 ],
             [ 1,0,0,0,0,0,1 ],
             [ 1,1,1,1,1,0,1 ] ]
mazeSize = (6,7) :: MazeSize

genMaze :: MazeData -> Either String [Node]
genMaze [] = Left "Empty MazeData Provided"
genMaze theList = generateFrom 0
    where
        generateFrom :: Int -> Either String [Node]
        generateFrom !pos = case nodeLine (pos,0) of
                              Left msg -> Left msg
                              Right nodes -> case generateFrom $ pos+1 of
                                               Left msg -> Right nodes
                                               Right nodes' -> Right $ nodes ++ nodes'

        nodeLine :: Position -> Either String [Node]
        nodeLine (i,j) 
          | i >= length theList || i < 0 = Left "column out of range"
          | j >= length (theList !! i) || j < 0 = Left "row out of range"
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
                  | i+1 >= length theList = Nothing
                  | theList !! (i+1) !! j == 1 = Nothing
                  | otherwise = Just (j,i+1)

                left'
                  | j-1 < 0 = Nothing 
                  | theList !! i !! (j-1) == 1 = Nothing
                  | otherwise = Just (j-1,i)

                right' 
                  | j+1 >= length theList = Nothing
                  | theList !! i !! (j+1) == 1 = Nothing
                  | otherwise = Just (j+1,i)
