module Main where

import Tests.Tree
import Structs.Tree
import Structs.Heap


nFullRows :: Int -> Tree Int
nFullRows n = balance $ foldl insert Empty [0..(2 ^ n - 2)]


main :: IO ()
main = do
    testInsert;
    testSearch;
    testEq;
