module Main where

import Structs.Tree
import Structs.Heap


nFullRows :: Int -> Tree Int
nFullRows n = rebalance $ foldl insert Empty [0..(2 ^ n - 2)]


main :: IO ()
main = do
    putStrLn ("size: " ++ show (size tree))
    print tree
    print $ flatten tree
    where
        tree = minHeapify $ nFullRows 4
