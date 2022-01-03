module Main where

import Structs.Tree
import Structs.Heap


nFullRows :: Int -> Tree Int
nFullRows n = balance $ foldl insert Empty [0..(2 ^ n - 2)]


main :: IO ()
main = do
    print tree;
    where
        tree = minHeapify $ nFullRows 4
