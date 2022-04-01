module Data.Tree where

import Text.Printf

import Control.Applicative


type CompFn a = (a -> a -> Ordering)


data Tree a
    = Empty
    | Node {
        value :: a,
        left  :: Tree a,
        right :: Tree a
    }
    deriving (Show)



new :: a -> Tree a
new a = Node a Empty Empty


-- |Child Function MAP Tree: fmap a `Tree a -> Tree a` 
-- function to all children
cfmapt :: (Tree a -> Tree a) -> Tree a -> Tree a
cfmapt _ Empty = Empty
cfmapt f (Node v le ri) = Node v (cfmapt f le) (cfmapt f ri)


insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = new x
insert x t@(Node v le ri) = case compare x v of
    EQ -> t
    LT -> t { left = insert x le }
    GT -> t { right = insert x ri }


-- Worst case: O(log n)
search :: (Ord a) => a -> Tree a -> Tree a
search _ Empty = Empty
search x t@(Node v le ri) = case compare x v of
    EQ -> t
    LT -> search x le
    GT -> search x ri


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
balance _ = error "balance"


-- O(n)
flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node x le ri) = flatten le ++ (x:(flatten ri))


pretty :: (Show a) => Tree a -> String
pretty = impl 0
    where
        nTabs :: Int -> String
        nTabs n = replicate n '\t'
        impl :: (Show a) => Int -> Tree a -> String
        impl n (Node x Empty Empty) =
            printf "%sNode (%s)\n" (nTabs n) (show x)
        impl n (Node x le ri) =
            printf "%s%sNode (%s)\n%s" (impl' le) ts (show x) (impl' ri)
                where
                    ts = nTabs n
                    impl' = impl (n + 1)
        impl n Empty = printf "%sEmpty\n" (nTabs n)
        -- impl n t = printf "%s%s\n" (nTabs n) (show t)


instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x le ri) = Node (f x) (fmap f le) (fmap f ri)

    _ <$ Empty = Empty
    y <$ (Node _ le ri) = Node y (y <$ le) (y <$ ri)


instance Foldable Tree where
    foldr _ a Empty = a
    foldr f a (Node x le ri) = foldr f (f x (foldr f a le)) ri


instance Applicative Tree where
    pure = new
    (Node f fl fr) <*> (Node x le ri)
        = Node (f x) (fl <*> le) (fr <*> ri)
    _ <*> _ = Empty


instance Alternative Tree where
    empty = Empty
    Empty <|> t = t
    t <|> _ = t


instance (Eq t) => Eq (Tree t) where
    Empty == Empty = True
    (Node av al ar) == (Node bv bl br)
        = (av == bv) && (al == bl) && (ar == br)
    _ == _ = False
