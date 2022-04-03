module Main where

import Prelude hiding (lookup)

import Control.Monad (foldM_)
import Data.Trie
import System.Exit
import Tests.Tools

default (Int, Double)



test_lookup :: Test String (Maybe Int)
test_lookup = Test {
        testName = "lookup",
        testFunc = \s -> lookup s trie,
        testAsserter = (==),
        testPrintIn = \_ -> print,
        testPrintOut = \_ -> print,
        testPrintSucc = False
    }

list :: [(String, Int)]
list = zip ["<<", "&", "|", "^", ">>"] [0..]
-- list = zip ["video","possible","dump","achieve",
--         "ankle","job","referee","area",
--         "total","|","silver",
--         "poison","<<","age","extinct",
--         "!","obese","^","wage","we",
--         "tooth","&","^$#*","spill",
--         "opposed","%","appendix",
--         "ridge","miner","he"] [0..]

trie :: Trie Int
trie = foldr (uncurry insert) empty list

main :: IO ()
main = do
    print (fmap fst $! assocs trie)
    foldM_ (\lst (s, i) -> case lst of
        Fail -> printTrie trie >> exitFailure
        _ -> runTestUsing_ test_lookup s (Just i)
        ) Pass_ list
    exitSuccess
