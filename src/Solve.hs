module Solve where

import Control.Monad
import Data.List
import Data.Bool.HT

import Types
import Utils

isDeadEnd :: [Node] -> MazeSize -> Position -> Maybe Node -> Bool
isDeadEnd [] _ _ _ = True
isDeadEnd _ _ _ Nothing = True
isDeadEnd mazeDat (x,y) fromNode (Just node) =
    case position node of
      (0,y') -> False
      (x',0) -> False
      (x',y') -> let  recurs = isDeadEnd mazeDat (x,y) (x',y')
                      mbPosToMbNode mbPos = case mbPos of
                                              Nothing -> Nothing
                                              Just pos -> case nodeat pos mazeDat of
                                                            Left msg -> Nothing
                                                            Right aNode -> Just aNode
                 in 
                   not (x' == x || y' == y) 
                   && (up node /= Just fromNode ?: (recurs . mbPosToMbNode . up $ node, True))
                   && (down node /= Just fromNode ?: (recurs . mbPosToMbNode . down $ node, True))
                   && (left node /= Just fromNode ?: (recurs . mbPosToMbNode . left $ node, True))
                   && (right node /= Just fromNode ?: (recurs . mbPosToMbNode . right $ node, True))

removeDeadPaths :: [Node] -> MazeSize -> [Node]
removeDeadPaths [] _ = []
removeDeadPaths nodes size = removeDuplicateNodes $ follow first MyUp []
    where 
        first = Just . head $ nodes
        isDead dir node = isDeadEnd nodes size (position node) (mbPosToMbNode . dir $ node)
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
        follow (Just node) dir blocked = node 
                    :   (if isblocked up node || dir == MyUp || isDead up node
                            then []
                            else followPos up node MyDown (node : blocked)) 
                    ++  (if isblocked down node || dir == MyDown || isDead down node
                            then []
                            else followPos down node MyUp (node : blocked)) 
                    ++  (if isblocked left node || dir == MyLeft || isDead left node
                            then []
                            else followPos left node MyRight (node : blocked))
                    ++  (if isblocked right node || dir == MyRight || isDead right node
                            then []
                            else followPos right node MyLeft (node : blocked))
            where 
                isblocked dir node = case dir node of
                                   Nothing -> True
                                   Just pos -> case nodeat pos blocked of
                                                 Left msg -> False
                                                 Right node -> True

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


