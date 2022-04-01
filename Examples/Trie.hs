module Main where

import Prelude hiding (lookup)

import Control.Monad (foldM, forM)
-- import Data.List ((\\))

import Data.Trie.Strict


default (Int, Double)


list :: [(String, Int)]
list = zip [
        "queer",
        "i",
        "apparel",
        "in",
        "femboy",
        "inn",
        "apple",
        "apply"
    ] [0..]
-- list = [
--         ("A", 15),
--         ("to", 7),
--         ("tea", 3),
--         ("tee", 8),
--         ("ted", 4),
--         ("ten", 12),
--         ("i", 11),
--         ("in", 5),
--         ("inn", 9)
--     ]


-- trie :: Trie Int
-- trie = fromList list


main :: IO ()
main = do
    putStrLn "list: "
    forM list $! \pair ->
        putStrLn $! "    " ++ show pair
    putStrLn "\nSteps:"
    foldM (\trie (str, n) -> do
            putStrLn $! "Insertion: " ++ show (str, n)
            let trie' = insert str n trie
            printTrie trie'
            putStrLn $! replicate 25 '~' ++ "\n"
            return $! trie')
        empty list
    return ()
    -- putStrLn "fromList:"
    -- printTrie trie
    -- putStrLn $! "\n\ntoList:"
    -- putStrLn $! "list == toList (fromList list)?: "
    --     ++ show (null (list \\ toList trie))
    -- putStrLn "\nshow: "
    -- print trie
