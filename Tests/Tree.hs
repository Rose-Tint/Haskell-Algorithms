{-# OPTIONS_GHC -O0 #-}

module Tests.Tree where

import Tests.Test
import Structs.Tree


testInsert = do
    putStrLn "Tree: insert";
    assertEq "1st" (insert Empty 0) (newTree 0);
    assertEq "2nd" second second';
    assertEq "3rd" third third';
    assertEq "4th" fourth fourth';
        where
            second = insert (newTree 0) 1
            second' = Node 0 Empty (newTree 1)
            third = insert (insert (newTree 1) 0) 2
            third' = Node 1 (newTree 0) (newTree 2)
            fourth = insert (insert (insert (newTree 2) 0) 3) 1
            fourth' = Node 2 (Node 0 Empty (newTree 1)) (newTree 3)


testSearch = do
    putStrLn "Tree: search"
    assertEq "->  4" first first'
    assertEq "->  8" second second'
    assertEq "->  2" third third'
    assertEq "->  6" fourth fourth'
    assertEq "-> 10" fifth fifth'
        where
            tree = foldl insert Empty [6,0,2,5,4,7,3,1,8,9]
            search' = search tree
            first = search' 4
            first' = Node 4 (newTree 3) Empty
            second = search' 8
            second' = Node 8 Empty (newTree 9)
            third = search' 2
            third' = Node 2 (newTree 1) (Node 5 (Node 4 (newTree 3) Empty) Empty)
            (fourth,fourth') = (search' 6, tree)
            (fifth,fifth') = (search' 10, Empty)


testEq = do
    putStrLn "Tree: (==)";
    assertEq "-> single" first first;
    assertEq "-> left  " second second;
    assertEq "-> both  " third third;
    assertEq "-> right " fourth fourth;
    assertEq "-> empty " fifth fifth;
        where
            first = newTree 0
            second = Node 0 (newTree 1) Empty
            third = Node 0 (newTree 1) (newTree 2)
            fourth = Node 0 Empty (newTree 1)
            fifth :: Tree Int
            fifth = Empty

