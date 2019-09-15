module Solve where

import           Control.Monad
import           Data.List
import           Data.Bool

import           Types -- src/Types.hs
import           Utils -- src/Utils.hs

isDeadEnd :: Nodes -> Nodes -> MazeSize -> Position -> Maybe Node -> Bool
isDeadEnd [] _ _ _ _       = True
isDeadEnd _  _ _ _ Nothing = True
isDeadEnd mazeDat blocked (x, y) fromNode' (Just node)
    | node `elem` blocked = False
    | otherwise           = isDead node blocked fromNode' 0
  where
    isDead node' blocked' fromNode n
        | node' `elem` blocked' = False
        | isOpen node' = False
        | otherwise = case position node' of
            (x', y') ->
                let
                    recurs mbNode = case mbNode of
                        Nothing  -> True
                        Just new -> isDead new
                                           (node' : blocked')
                                           (position node')
                                           (n + 1)
                    same = node == node' || (position node') == fromNode'
                    mbPosToMbNode mbPos = case mbPos of
                        Nothing  -> Nothing
                        Just pos -> case nodeat pos mazeDat of
                            Left  msg   -> Nothing
                            Right aNode -> Just aNode
                in
                    if same && n /= 0
                        then True
                        else
                            (if (up node' /= Just fromNode)
                                then (recurs . mbPosToMbNode . up $ node')
                                else True
                            )
                            && (if (down node' /= Just fromNode)
                                   then recurs . mbPosToMbNode . down $ node'
                                   else True
                               )
                            && (if (left node' /= Just fromNode)
                                   then recurs . mbPosToMbNode . left $ node'
                                   else True
                               )
                            && (if (right node' /= Just fromNode)
                                   then recurs . mbPosToMbNode . right $ node'
                                   else True
                               )

exitOfPath :: Node -> Direction -> Direction
exitOfPath node dir | not $ isPath node = dir
                    | otherwise         = outdir
  where
    outdir = theOtherDirof node dir
    notNothing :: [Maybe a] -> Maybe Int
    notNothing [] = Nothing
    notNothing (x : xs) =
        (case x of
            Nothing -> case notNothing xs of
                Nothing -> Nothing
                Just x  -> Just $ 1 + x
            Just j  -> Just 1
        )
    theOtherDirof :: Node -> Direction -> Direction
    theOtherDirof (Node _ u d l r) dir = case dir of
        DUp    -> case notNothing [d, l, r] of
            Just 1  -> DDown
            Just 2  -> DLeft
            Just 3  -> DRight
            Nothing -> DUp
        DDown  -> case notNothing [u, l, r] of
            Just 1  -> DUp
            Just 2  -> DLeft
            Just 3  -> DRight
            Nothing -> DDown
        DLeft  -> case notNothing [u, d, r] of
            Just 1  -> DUp
            Just 2  -> DDown
            Just 3  -> DRight
            Nothing -> DLeft
        DRight -> case notNothing [u, d, l] of
            Just 1  -> DUp
            Just 2  -> DDown
            Just 3  -> DLeft
            Nothing -> DRight

removeDeadPaths :: Nodes -> MazeSize -> (Int, Direction) -> Nodes
removeDeadPaths []    _    _               = []
removeDeadPaths nodes size (starti, mydir) = removeDuplicateNodes
    $ follow first mydir []
  where
    first = Just $ nodes !! starti
    isDead block dir node =
        isDeadEnd nodes block size (position node) (mbPosToMbNode . dir $ node)
    followPos dir nod from = follow (mbPosToMbNode . dir $ nod) from
    mbPosToMbNode mbPos = case mbPos of
        Nothing  -> Nothing
        Just pos -> case nodeat pos nodes of
            Left  msg   -> Nothing
            Right aNode -> Just aNode

    removeDuplicateNodes :: [Node] -> [Node]
    removeDuplicateNodes [] = []
    removeDuplicateNodes (n : ns) =
        let newNodes = removeDuplicateNodes ns
        in  if (n `elem` newNodes) then newNodes else (n : newNodes)

    follow :: Maybe Node -> Direction -> Nodes -> Nodes
    follow Nothing _ _ = []
    follow (Just node) dir blocked
        | node `elem` blocked
        = []
        | not . isPath $ node
        = toblocks !! 4
        | -- if it's not a path, return all necessary routes
          otherwise
        = node
            : (case exitOfPath node dir of
                               -- if it is a path, only check the exit route
                  DUp ->
                      if (  up node
                         == Just (-1, -1)
                         || isDead (head toblocks) up node
                         )
                      then
                          []
                      else
                          followPos up node DDown (head toblocks)
                  DDown ->
                      if (  down node
                         == Just (-1, -1)
                         || isDead (head toblocks) down node
                         )
                      then
                          []
                      else
                          followPos down node DUp (head toblocks)
                  DLeft ->
                      if (  left node
                         == Just (-1, -1)
                         || isDead (head toblocks) left node
                         )
                      then
                          []
                      else
                          followPos left node DRight (head toblocks)
                  DRight ->
                      if (  right node
                         == Just (-1, -1)
                         || isDead (head toblocks) right node
                         )
                      then
                          []
                      else
                          followPos right node DLeft (head toblocks)
              )
      where
                -- the different blocked lists for each path
        toblocks =
            [ node : blocked
            , (toblocks !! 0) ++ up'
            , (toblocks !! 1) ++ down'
            , (toblocks !! 2) ++ left'
            , (toblocks !! 3) ++ right'
            ]

        -- nodes in the path above the current node
        up'
            = (if (  dir
                  == DUp
                  || up node
                  == Just (-1, -1)
                  || isDead (head toblocks) up node
                  )
               then
                   []
               else
                   followPos up node DDown (head toblocks)
              )
        -- nodes in the path bellow the current node
        down' =
            (if (  dir
                == DDown
                || down node
                == Just (-1, -1)
                || isDead (toblocks !! 1) down node
                )
                then []
                else followPos down node DUp (toblocks !! 1)
            )
        -- nodes in the path left of the current node
        left' =
            (if (  dir
                == DLeft
                || left node
                == Just (-1, -1)
                || isDead (toblocks !! 2) left node
                )
                then []
                else followPos left node DRight (toblocks !! 2)
            )
        -- nodes in the path right of the current node
        right' =
            (if (  dir
                == DRight
                || right node
                == Just (-1, -1)
                || isDead (toblocks !! 3) right node
                )
                then []
                else followPos right node DLeft (toblocks !! 3)
            )
