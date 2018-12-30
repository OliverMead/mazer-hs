module Lib
    where

type Pos = (Integer, Integer)

data Node = Node {  position :: Pos,
                    up :: Maybe Pos,
                    down :: Maybe Pos,
                    left :: Maybe Pos,
                    right :: Maybe Pos 
                 } deriving (Show, Eq)

minX = 0
minY = 0

maxX = 9
maxY = 9

maze :: [Node]
maze = [Node (x,y) 
             (if y+1 <= maxY then (Just (x,y+1)) else Nothing)
             (if y-1 >= minY then (Just (x,y-1)) else Nothing)
             (if x-1 >= minX then (Just (x-1,y)) else Nothing)
             (if x+1 <= maxX then (Just (x+1,y)) else Nothing)
               | x <- [minX..maxX], y <- [minY..maxY]]

printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x:xs) = do
    print x
    printList xs

nodeat :: Pos -> [Node] -> Either String Node
-- finds first node in set of nodes at given position
nodeat pos [] = Left ("No node at " ++ show pos)
nodeat pos (x:xs) = if (position x == pos) 
                        then Right x 
                        else nodeat pos xs
