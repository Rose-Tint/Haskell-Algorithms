module Structs.Tree where

import Control.Applicative

type CompFn a = (a -> a -> Ordering)


data Tree a
    = Empty
    | Node {
        value :: a,
        left  :: Tree a,
        right :: Tree a
    }


newTree :: a -> Tree a
newTree a = Node a Empty Empty


-- Worst case: O(log n)
insert :: (Ord a) => Tree a -> a -> Tree a
insert t y = finsert compare t y


finsert :: CompFn a -> Tree a -> a -> Tree a
finsert _ Empty y = newTree y
finsert f t@(Node x le ri) y = case f y x of
    EQ -> t
    LT -> Node x (finsert f le y) ri
    GT -> Node x le (finsert f ri y)


-- Worst case: O(log n)
search :: (Ord a) => Tree a -> a -> Tree a
search = fsearch compare


fsearch :: CompFn a -> Tree a -> a -> Tree a
fsearch _ Empty _ = Empty
fsearch f t@(Node x le ri) y = case f y x of
    EQ -> t
    LT -> fsearch f le y
    GT -> fsearch f ri y


-- O(log n)
height :: Tree b -> Int
height Empty = 0
height (Node _ Empty Empty) = 0
height (Node _ le Empty) = 1 + height le
height (Node _ Empty ri) = 1 + height ri
height (Node _ le ri) = 1 + max (height le) (height ri)


-- O(n)
invert :: (Ord a) => Tree a -> Tree a
invert Empty = Empty
invert (Node x le ri) = Node x (invert ri) (invert le)


-- O(n)
fmerge :: (b -> c -> d) -> Tree b -> Tree c -> Tree d
fmerge _ Empty Empty = Empty
fmerge f (Node lx ll lr) (Node rx rl rr)
    = Node (f lx rx) (fmerge f ll rl) (fmerge f lr rr)
fmerge _ Empty _
    = error "Tree Tree -> fmerge: Trees do not match in structure"
fmerge _ _ Empty
    = error "Tree Tree -> fmerge: Trees do not match in structure"


-- O(n)
size :: Tree a -> Int
size Empty = 0
size (Node _ le ri) = 1 + size le + size ri


-- Too complicated...
balance :: Tree a -> Tree a
balance Empty = Empty
balance t@(Node x le ri)
    | (abs st < 2) = t
    | (st >= 2 && sl /= -1) = rotateR t
    | (st >= 2 && sl == -1)
        = rotateR $ Node x (rotateL le) ri
    | (st <= -2 && sr /= 1)
        = rotateL $ Node x le ri
    | (st <= -2 && sr == 1)
        = rotateL $ Node x le (rotateR ri)
        where
            slope Empty = 0
            slope (Node _ le' ri') = height le' - height ri'
            (st,sl,sr) = (slope t, slope le, slope ri)
            rotateL (Node x' le' (Node rx rl rr))
                = balance $ Node rx (balance $ Node x' le' rl) (balance rr)
            rotateL _ = error "rotateL"
            rotateR (Node x' (Node lx ll lr) ri')
                = balance $ Node lx (balance ll) (balance $ Node x' lr ri')
            rotateR _ = error "rotateR"
balance _ = error "Balance"


-- O(n)
flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node x le ri) = flatten le ++ (x:(flatten ri))


instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x le ri) = Node (f x) (fmap f le) (fmap f ri)

    _ <$ Empty = Empty
    y <$ (Node _ le ri) = Node y (y <$ le) (y <$ ri)


instance Foldable Tree where
    foldr _ a Empty = a
    foldr f a (Node x le ri) = foldr f (f x (foldr f a le)) ri


instance Applicative Tree where
    pure = newTree
    (Node f fl fr) <*> (Node x le ri)
        = Node (f x) (fl <*> le) (fr <*> ri)
    _ <*> _ = Empty


instance Alternative Tree where
    empty = Empty
    Empty <|> t = t
    t <|> _ = t


instance (Eq t) => Eq (Tree t) where
    Empty == Empty = True
    (Node av al ar) == (Node bv bl br) = (av == bv) && (al == bl) && (ar == br)
    _ == _ = False


instance (Show a) => Show (Tree a) where
    show = impl 0
        where
            nTabs :: Int -> String
            nTabs n = replicate n '\t'
            impl :: (Show a) => Int -> Tree a -> String
            impl n Empty = nTabs n ++ "[_]\n"
            impl n (Node x Empty Empty) = nTabs n ++ "[" ++ show x ++ "]\n"
            impl n (Node x le ri)
                = impl (n + 1) le ++ nTabs n ++ "[" ++ show x ++ "]\n" ++ impl (n + 1) ri
