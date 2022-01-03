module Structs.Heap where

import Structs.Tree


class MinHeap a where
    minMoveDown :: (Ord b) => a b -> a b
    minHeapify :: (Ord b) => a b -> a b


class MaxHeap a where
    maxMoveDown :: (Ord b) => a b -> a b
    maxHeapify :: (Ord b) => a b -> a b


instance MinHeap Tree where
    minMoveDown Empty = Empty
    minMoveDown t@(Node _ Empty Empty) = t
    minMoveDown t@(Node x (Node lx ll lr) Empty)
        = Node sm (minMoveDown $ Node lg ll lr) Empty
            where (sm, lg) = if x < lx then (x, lx) else (lx, x)
    minMoveDown t@(Node x le@(Node lx ll lr) ri@(Node rx rl rr))
        | (x <= lx && x <= rx) = t
        | (lx <= x && lx <= rx) = Node lx (minMoveDown $ Node x ll lr) ri
        | (rx <= x && rx <= lx) = Node rx le (minMoveDown $ Node x rl rr)

    minHeapify Empty = Empty
    minHeapify (Node x le ri)
        = minMoveDown $ Node x (minHeapify le) (minHeapify ri)


instance MaxHeap Tree where
    maxMoveDown Empty = Empty
    maxMoveDown t@(Node _ Empty Empty) = t
    maxMoveDown t@(Node x (Node lx ll lr) Empty)
        = Node lg (maxMoveDown $ Node sm ll lr) Empty
            where (sm, lg) = if x < lx then (x, lx) else (lx, x)
    maxMoveDown t@(Node x le@(Node lx ll lr) ri@(Node rx rl rr))
        | (x >= lx && x >= rx) = t
        | (lx >= x && lx >= rx) = Node lx (maxMoveDown $ Node x ll lr) ri
        | (rx >= x && rx >= lx) = Node rx le (maxMoveDown $ Node x rl rr)

    maxHeapify Empty = Empty
    maxHeapify (Node x le ri)
        = maxMoveDown $ Node x (maxHeapify le) (maxHeapify ri)
