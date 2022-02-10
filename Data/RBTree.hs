module Data.RBTree where

import Data.Tree (CompFn)


data RBTree a
    = Empty
    | Red {
        value :: a,
        left  :: RBTree a,
        right :: RBTree a
    }
    | Black {
        value :: a,
        left  :: RBTree a,
        right :: RBTree a
    }


red :: a -> RBTree a
red a = Red a Empty Empty


black :: a -> RBTree a
black a = Black a Empty Empty


toBlack :: RBTree a -> RBTree a
toBlack Empty = Empty
toBlack t = Black (value t) (left t) (right t)


toRed :: RBTree a -> RBTree a
toRed Empty = Empty
toRed t = Red (value t) (left t) (right t)


isEmpty :: RBTree a -> Bool
isEmpty Empty = True
isEmpty _ = False


isLeaf :: RBTree a -> Bool
isLeaf (Red _ Empty Empty) = True
isLeaf (Black _ Empty Empty) = True
isLeaf _ = False


isRed :: RBTree a -> Bool
isRed (Red _ _ _) = True
isRed _ = False


isBlack :: RBTree a -> Bool
isBlack Empty = True
isBlack (Black _ _ _) = True
isBlack _ = False


height :: RBTree a -> Int
height Empty = 0
height t
    | (isLeaf t)  = 0
    | (otherwise) = max (height le) (height ri)
    where
        (le, ri) = (left t, right t)


insert :: (Ord a) => a -> RBTree a -> RBTree a
insert = finsert compare


finsert :: CompFn a -> a -> RBTree a -> RBTree a
finsert = repaint . finsert'


finsert' :: CompFn a -> a -> RBTree a -> RBTree a
finsert' _ a Empty = black a
finsert' f a t
    | (isLeaf t)  = case f a (value t) of
        GT -> t { right = red a }
        _  -> t { left = red a }
    | (otherwise) = case f a (value t) of
        GT -> t { right = finsert' f a (right t) }
        _  -> t { left = finsert' f a (left t) }
