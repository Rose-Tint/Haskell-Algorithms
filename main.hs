module Main where

import Tests.Tree
import Structs.Tree
import Structs.Heap


nFullRows :: Int -> Tree Int
nFullRows n = balance $ foldl insert Empty [0..(2 ^ n - 2)]


main :: IO ()
main = do
    -- print tree
    testInsert; testSearch; testEq;
    where
        tree = foldl insert Empty [6,0,2,5,4,7,3,1,8,9]
        third = search tree 2
        third' = Node 2 (newTree 7) (Node 8 (newTree 9) Empty)
