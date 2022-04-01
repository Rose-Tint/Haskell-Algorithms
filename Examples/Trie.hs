module Main where

import Prelude hiding (lookup)

import Control.Monad (foldM, mapM)
import Data.List ((\\), sortOn)
import Text.Printf (printf)

import Data.Trie.Strict


default (Int, Double)


list :: [(String, Int)]
list = zip [
        -- "i",
        "apple",
        "apparel",
        -- "in",
        -- "inn",
        "apply",
        "app",
        "apart",
        "apps",
        "appetizer",
        "appeal"
    ] [0..]


-- trie :: Trie Int
-- trie = fromList list


main :: IO ()
main = do
    -- putStrLn "list: "
    -- forM list $! \pair ->
    --     putStrLn $! "    " ++ show pair
    -- putStrLn "\nSteps:"
    trie <- foldM (\trie (str, n) -> do
            putStrLn $! "Insertion: " ++ show (str, n)
            let trie' = insert str n trie
            printTrie trie'
            putStrLn $! replicate 35 '~'
            return $! trie')
        empty list
    putStrLn "\n\nfromList:"
    printTrie trie
    putStrLn $! "\n\ntoList:"
    let list' = toList trie
    mapM (\(a, b) ->
        printf "%-16s, %s\n" (show a) (show b)) $!
        zip list (sortOn snd list')
    putStrLn $! "list == toList (fromList list)?: "
        ++ show (null (list \\ list'))
    -- putStrLn "\nshow: "
    -- print trie
    return ()
