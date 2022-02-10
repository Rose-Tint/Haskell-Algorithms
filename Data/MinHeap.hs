module Data.MinHeap (
    module Data.Tree,
    insert,
    finsert,
    balance,
    -- fflatten,
    heapify,
    fheapify
) where

import Data.Tree hiding (
    insert,
    finsert,
    balance-- ,
    -- flatten
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


-- fflatten :: CompFn a -> Tree a -> [a]
-- fflatten _ Empty = []
-- fflatten f (Node x le@(Node lx _ _) ri@(Node rx _ _))
--     = case f lx rx of
--         EQ -> [x, lx]
--               ++ fflatten f le
--               ++ fflatten f ri
--         LT -> [x, lx, rx]
--               ++ fflatten f le
--               ++ fflatten f ri
--         GT -> [x, rx, lx]
--               ++ fflatten f ri
--               ++ fflatten f le


heapify :: (Ord a) => Tree a -> Tree a
heapify t = fheapify compare t


fheapify :: CompFn a -> Tree a -> Tree a
fheapify _ Empty = Empty
fheapify f (Node x le ri) = sift $ Node x (fheapify f le) (fheapify f ri)
    where
        sift (Node x' le'@(Node lx ll lr) Empty)
            | (lte x' lx) = Node x' (sift le') Empty
            | otherwise  = Node lx (sift $ Node x' ll lr) Empty
        sift (Node x' Empty ri') = sift $ Node x' ri' Empty
        sift t@(Node x' le'@(Node lx ll lr) ri'@(Node rx rl rr))
            | (lte x' lx && lte x' rx)  = t
            | (lte lx x' && lte lx rx) = Node lx (sift $ Node x' ll lr) ri'
            | (lte rx x' && lte rx lx) = Node rx le' (sift $ Node x' rl rr)
        sift t = t
        lte a b = f a b `elem` [LT, EQ]
