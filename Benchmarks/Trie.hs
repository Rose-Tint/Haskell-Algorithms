{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Trie


default (Int)


list :: [(String, Int)]
list = zip ["video","possible","dump","achieve",
        "ankle","job","referee","area",
        "total","consensus","silver",
        "poison","admit","age","extinct",
        "harmony","obese","boot","wage","we",
        "tooth","bleed","stake","spill",
        "opposed","definite","appendix",
        "ridge","miner","he"] [0..]


main :: IO ()
main = do
    let !_ = fromList list
    return ()
