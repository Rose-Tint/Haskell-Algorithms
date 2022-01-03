module Main where

import Structs.Tree
import Structs.Heap


nFullRows :: Int -> Tree Int
nFullRows n = rebalance $ foldl insert Empty [0..(2 ^ n - 2)]


main :: IO ()
main = print minHeap
    where
        tree = nFullRows 4
        minHeap = minHeapify tree
