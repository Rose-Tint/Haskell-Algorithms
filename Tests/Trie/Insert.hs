module Main where

import Data.Array ((//))

import Data.Trie.Internal
import System.Exit
import Tests.Tools

default (Int, Double)



insert_test :: Test ((String, Int), Trie Int) (Trie Int)
insert_test = Test {
        testName = "insert",
        testFunc = \((s, n), t) -> insert s n t,
        testAsserter = (==),
        testPrintIn = \_ -> print . fst,
        testPrintOut = \_ _ -> putStrLn "",
        testPrintSucc = False
}


insert_values :: [((String, Int), Trie Int)]
insert_values = [
        (("apple", 0), singleton "apple" 0),
        (("apparel", 1), Link (newChildren // [
            ('a', singleton "rel" 1),
            ('l', singleton "e" 0)
            ]) "app"),
        (("apply", 2), Link (newChildren // [
            ('a', singleton "rel" 1),
            ('l', Link (newChildren // [
                ('e', node 0),
                ('y', node 2)
                ]) "")
            ]) "app"),
        (("app", 3), Link (oneChild 'p' $
            Node (newChildren // [
                ('a', singleton "rel" 1),
                ('l', Link (newChildren // [
                    ('e', node 0),
                    ('y', node 2)
                    ]) "")
                ]
            ) 3) "ap"),
        (("inn", 4), Link (newChildren // [
                ('a', Link (oneChild 'p' $
                    Node (newChildren // [
                        ('a', singleton "rel" 1),
                        ('l', Link (newChildren // [
                            ('e', node 0),
                            ('y', node 2)
                            ]) "")
                        ]
                    ) 3) "p"),
                ('i', Link (oneChild 'n' (node 4)) "n")
            ]) []),
        (("i", 5), Link (newChildren // [
                ('a', Link (oneChild 'p' $
                    Node (newChildren // [
                        ('a', singleton "rel" 1),
                        ('l', Link (newChildren // [
                            ('e', node 0),
                            ('y', node 2)
                            ]) "")
                        ]
                    ) 3) "p"),
                ('i', Node (oneChild 'n'
                    (Link (oneChild 'n' (node 4)) [])) 5)
            ]) [])
    ]


main :: IO ()
main = do
    insert_res <- foldTests insert_test empty insert_values
    case insert_res of
        Fail -> exitFailure
        _ -> exitSuccess
