module Main where

import Data.Tree


main :: IO ()
main = putStrLn $ prettyTree tree
    where
        tree :: Tree Int
        tree = 
            5 # (9 # 12
                <^> 15)
            |^| newTree 7
