module Data.MinHeap (
    module Data.Tree,
    insert,
    balance,
    heapify
) where

import Data.Tree hiding (
    insert,
    balance
    )
import qualified Data.Tree as T (
    insert,
    balance
    )


insert :: (Ord a) => a -> Tree a -> Tree a
insert a Empty = new a
insert a t = heapify $ T.insert a t


balance :: (Ord a) => Tree a -> Tree a
balance Empty = Empty
balance t = heapify $ T.balance t


heapify :: (Ord a) => Tree a -> Tree a
heapify Empty = Empty
heapify tree = sift (cfmapt heapify tree)
    where
        sift :: (Ord a) => Tree a -> Tree a
        sift (Node x le@(Node lx ll lr) Empty)
            | (x <= lx)
                = Node x (sift le) Empty
            | (otherwise)
                = Node lx (sift $ Node x ll lr) Empty
        sift (Node x Empty ri) = sift $ Node x ri Empty
        sift t@(Node x le@(Node lx ll lr) ri@(Node rx rl rr))
            | (x  <= lx && x  <= rx) = t
            | (lx <= x  && lx <= rx)
                = Node lx (sift $ Node x ll lr) ri
            | (rx <= x  && rx <= lx)
                = Node rx le (sift $ Node x rl rr)
        sift t = t
