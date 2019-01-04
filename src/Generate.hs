module Generate where

import Control.Monad
import Data.List
import Data.Bool.HT

import Types

mazeData :: [[Int]]
mazeData = [ [ 1,0,1,1,1 ],
             [ 1,0,1,0,1 ],
             [ 1,0,0,0,1 ],
             [ 1,0,1,0,1 ],
             [ 1,1,1,0,1 ] ]

genMaze :: MazeData -> Either String [Node]
genMaze [] = Left "Empty MazeData Provided"
genMaze theList = generateFrom theList 0
    where
        generateFrom :: MazeData -> Int -> Either String [Node]
        generateFrom xs !i = case nodeLine xs i 0 of
                              Left msg -> Left msg
                              Right nodes -> case generateFrom xs (i+1) of
                                               Left msg -> Right nodes
                                               Right nodes' -> Right $ nodes ++ nodes'

        nodeLine :: MazeData -> Int -> Int -> Either String [Node]
        nodeLine theList i j 
          | i >= length theList || i < 0 = Left "row out of range"
          | j >= length (theList !! i) || j < 0 = Left "column out of range"
          | otherwise = case makeNode i j of
                          Left msg -> nodeLine theList i $ j+1
                          Right node -> case nodeLine theList i $ j+1 of
                                          Left msg -> Right [node]
                                          Right nodes -> Right $ node : nodes
            where 
                makeNode :: Int -> Int -> Either String Node
                makeNode i j = case theList !! i !! j of
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
                          | theList !! i !! j-1 == 1 = Nothing
                          | otherwise = Just (j-1,i)

                        right' 
                          | j+1 >= length theList = Nothing
                          | theList !! i !! j+1 == 1 = Nothing
                          | otherwise = Just (j+1,i)
