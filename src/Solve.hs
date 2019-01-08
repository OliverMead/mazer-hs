module Solve where

import Control.Monad
import Data.List
import Data.Bool.HT

import Types
import Utils

isDeadEnd :: [Node] -> [Node] -> MazeSize -> Position -> Maybe Node -> Bool
isDeadEnd [] _ _ _ _ = True
isDeadEnd _ _ _ _ Nothing = True
isDeadEnd mazeDat blocked (x,y) fromNode' (Just node) 
  | node `elem` blocked = True
  | otherwise = isDead node blocked fromNode' 0
    where 
        isDead node' blocked' fromNode n 
          | node' `elem` blocked' = True
          | otherwise =
             case position node' of
              (0,y') -> False
              (x',0) -> False
              (x',y') -> let recurs mbNode = case mbNode of
                                               Nothing -> True
                                               Just new -> isDead new (node':blocked') (position node') (n+1)
                             same = node == node' || (position node') == fromNode'
                             mbPosToMbNode mbPos = 
                                case mbPos of
                                  Nothing -> Nothing
                                  Just pos -> case nodeat pos mazeDat of
                                                Left msg -> Nothing
                                                Right aNode -> Just aNode
                         in 
                            if same && n/=0 then True else ( not (x' == x || y' == y) 
                            && (up node' /= Just fromNode ?: (recurs . mbPosToMbNode . up $ node', True))
                            && (down node' /= Just fromNode ?: (recurs . mbPosToMbNode . down $ node', True))
                            && (left node' /= Just fromNode ?: (recurs . mbPosToMbNode . left $ node', True))
                            && (right node' /= Just fromNode ?: (recurs . mbPosToMbNode . right $ node', True)) )

isPath :: Node -> Bool
isPath (Node _ u d l r)
  | numnothings dirs == 2 = True
  | otherwise = False
    where 
        dirs = [u, d, l, r]
        numnothings :: [Maybe a] -> Int
        numnothings [] = 0
        numnothings (x:xs) = case x of
                               Nothing -> 1 + numnothings xs
                               Just j -> numnothings xs

exitOfPath :: Node -> Direction -> Direction
exitOfPath node dir
  | not $ isPath node = dir
  | otherwise = outdir
    where 
        outdir = theOtherDirof node dir
        notNothing :: [Maybe a] -> Maybe Int
        notNothing [] = Nothing
        notNothing (x:xs) = (case x of
                               Nothing -> case notNothing xs of
                                            Nothing -> Nothing
                                            Just x -> Just $ 1 + x
                               Just j -> Just 1)
        theOtherDirof :: Node -> Direction -> Direction
        theOtherDirof (Node _ u d l r) dir = case dir of
                                               MyUp -> case notNothing [d,l,r] of
                                                         Just 1 -> MyDown
                                                         Just 2 -> MyLeft
                                                         Just 3 -> MyRight
                                                         Nothing -> MyUp
                                               MyDown -> case notNothing [u,l,r] of
                                                           Just 1 -> MyUp
                                                           Just 2 -> MyLeft
                                                           Just 3 -> MyRight
                                                           Nothing -> MyDown
                                               MyLeft -> case notNothing [u,d,r] of
                                                           Just 1 -> MyUp
                                                           Just 2 -> MyDown
                                                           Just 3 -> MyRight
                                                           Nothing -> MyLeft
                                               MyRight -> case notNothing [u,d,l] of
                                                           Just 1 -> MyUp
                                                           Just 2 -> MyDown
                                                           Just 3 -> MyLeft
                                                           Nothing -> MyRight

removeDeadPaths :: [Node] -> MazeSize -> (Int,Direction) -> [Node]
removeDeadPaths [] _ _ = []
removeDeadPaths nodes size (starti,mydir) = removeDuplicateNodes $ follow first mydir []
    where 
        first = Just $ nodes !! starti
        isDead block dir node = isDeadEnd nodes block size (position node) (mbPosToMbNode . dir $ node)
        followPos dir nod from = follow (mbPosToMbNode . dir $ nod) from
        mbPosToMbNode mbPos = case mbPos of
                                Nothing -> Nothing
                                Just pos -> case nodeat pos nodes of
                                              Left msg -> Nothing
                                              Right aNode -> Just aNode

        removeDuplicateNodes :: [Node] -> [Node]
        removeDuplicateNodes [] = []
        removeDuplicateNodes (n:ns) = let newNodes = removeDuplicateNodes ns
                                      in n `elem` newNodes ?: (newNodes, n : newNodes)
        
        follow :: Maybe Node -> Direction -> [Node] -> [Node]
        follow Nothing _ _ = []
        follow (Just node) dir blocked 
          | node `elem` blocked = []
          | otherwise = toblocks !! 4
                where 
                    toblocks = [node : blocked,
                                (toblocks !! 0) ++ up',
                                (toblocks !! 1) ++ down',
                                (toblocks !! 2) ++ left',
                                (toblocks !! 3) ++ right' ]
                    up' =   (if (dir == MyUp || isDead (toblocks !! 0) up node)
                                then []
                                else followPos up node MyDown (toblocks !! 0)) 
                    down' = (if (dir == MyDown || isDead (toblocks !! 1) down node)
                                then []
                                else followPos down node MyUp (toblocks !! 1)) 
                    left' = (if (dir == MyLeft || isDead (toblocks !! 2) left node)
                                then []
                                else followPos left node MyRight (toblocks !! 2))
                    right'= (if (dir == MyRight || isDead (toblocks !! 3) right node)
                                then []
                                else followPos right node MyLeft (toblocks !! 3))

showSolved :: [Node] -> [Node] -> MazeSize -> IO ()
showSolved nodes solution (x,y) = showMazeFrom (0,0)
    where
        showMazeFrom :: Position -> IO ()
        showMazeFrom (x',y') 
          | x' > x = putStrLn "" >> showMazeFrom (0,y'+1) 
          | y' > y = return ()
          | otherwise = case nodeat (x',y') nodes of
                          Left msg -> putStr "█" >> showMazeFrom (x'+1,y')
                          Right node -> case nodeat (x',y') solution of
                                          Left msg -> putStr "#" >> showMazeFrom (x'+1, y')
                                          Right node -> putStr " " >> showMazeFrom (x'+1,y')


