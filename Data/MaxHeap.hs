module Data.MaxHeap (
    module Data.Tree,
    insert,
    finsert,
    balance,
    heapify,
    fheapify
) where

import Data.Tree hiding (
    insert,
    finsert,
    balance
    )
import qualified Data.Tree as T (
    insert,
    finsert,
    balance
    )


insert :: (Ord a) => Tree a -> a -> Tree a
insert t a = heapify $ T.insert t a


finsert :: CompFn a -> Tree a -> a -> Tree a
finsert f t a = fbalance f $ T.finsert f t a


balance :: (Ord a) => Tree a -> Tree a
balance t = heapify $ fbalance compare t


fbalance :: CompFn a -> Tree a -> Tree a
fbalance f t = fheapify f $ T.balance t


heapify :: (Ord a) => Tree a -> Tree a
heapify t = fheapify compare t


fheapify :: CompFn a -> Tree a -> Tree a
fheapify _ Empty = Empty
fheapify f (Node x le ri) = sift $ Node x (fheapify f le) (fheapify f ri)
    where
        sift (Node x' le'@(Node lx ll lr) Empty)
            | (gte x' lx) = Node x' (sift le') Empty
            | otherwise  = Node lx (sift $ Node x' ll lr) Empty
        sift (Node x' Empty ri') = sift $ Node x' ri' Empty
        sift t@(Node x' le'@(Node lx ll lr) ri'@(Node rx rl rr))
            | (gte x' lx && gte x' rx)  = t
            | (gte lx x' && gte lx rx) = Node lx (sift $ Node x' ll lr) ri'
            | (gte rx x' && gte rx lx) = Node rx le' (sift $ Node x' rl rr)
        sift t = t
        gte a b = f a b `elem` [GT, EQ]
