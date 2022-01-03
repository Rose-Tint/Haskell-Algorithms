module Structs.Tree (
    Tree(Empty, Node),
    value,
    left,
    right,
    newTree,
    insert,
    search,
    height,
    invert,
    fmerge,
    size,
    rebalance,
) where


data Tree a
    = Empty
    | Node {
        value :: a,
        left  :: Tree a,
        right :: Tree a
    }


newTree :: a -> Tree a
newTree a = Node a Empty Empty


insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty y = Node y Empty Empty
insert (Node x Empty Empty) y = Node x (Node y Empty Empty) Empty
insert (Node x le Empty) y = Node x le (Node y Empty Empty)
insert (Node x Empty ri) y = Node x ri (Node y Empty Empty)
insert t@(Node x le ri) y
    | (y == x) = t
    | (y < x) = Node x (insert le y) ri
    | (y > x) = Node x le (insert ri y)


search :: (Ord a) => Tree a -> a -> Tree a
search Empty y = Empty
search (Node x le ri) y
    | (y == x) = Node x le ri
    | (y < x) = search le y
    | (y > x) = search ri y


height :: Tree b -> Int
height Empty = 0
height (Node _ Empty Empty) = 0
height (Node _ le ri) = 1 + max (height le) (height ri)


invert :: (Ord a) => Tree a -> Tree a
invert Empty = Empty
invert (Node x le ri) = Node x (invert ri) (invert le)


fmerge :: (b -> c -> d) -> Tree b -> Tree c -> Tree d
fmerge _ Empty Empty = Empty
fmerge f (Node lx ll lr) (Node rx rl rr)
    = Node (f lx rx) (fmerge f ll rl) (fmerge f lr rr)
fmerge f Empty ri = error "Tree Tree -> fmerge: Trees do not match in structure"


size :: Tree a -> Int
size Empty = 0
size (Node _ le ri) = 1 + size le + size ri


slope :: Tree a -> Int
slope Empty = 0
slope (Node _ le ri) = height le - height ri


rebalance :: Tree a -> Tree a
rebalance Empty = Empty
rebalance t@(Node x le ri)
    | (abs sy < 2) = t
    | (sy >= 2 && sl /= -1) = rotR t
    | (sy >= 2 && sl == -1) = rotR $ Node x (rotL le) ri
    | (sy <= -2 && sr /= 1) = rotL $ Node x le ri
    | (sy <= -2 && sr == 1) = rotL $ Node x le (rotR ri)
        where
            slope Empty = 0
            slope (Node _ le ri) = height le - height ri
            (x,le,ri) = (value t, left t, right t)
            (sy,sl,sr) = (slope t, slope le, slope ri)
            rotL (Node _x _le (Node rx rl rr))
                = rebalance $ Node rx (rebalance $ Node _x _le rl) (rebalance rr)
            rotR (Node _x (Node lx ll lr) _ri)
                = rebalance $ Node lx (rebalance ll) (rebalance $ Node _x lr _ri)


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
    Empty <*> Empty = Empty
    (Node f fl fr) <*> (Node a al ar)
        = Node (f a) (fl <*> al) (fr <*> ar)
    _ <*> _ = Empty


instance Monad Tree where
    (Node x _ _) >>= f = f x


instance (Eq t) => Eq (Tree t) where
    Empty == Empty = True
    (Node av al ar) == (Node bv bl br) = (av == bv) && (al == bl) && (ar == br)
    _ == _ = False

    Empty /= Empty = False
    (Node av al ar) /= (Node bv bl br) = (av /= bv) || (al /= bl) || (ar /= br)
    _ /= _ = True


instance (Show a) => Show (Tree a) where
    show = showImpl 0


{- IMPLIMENTATION DETAILS -}


showImpl :: (Show a) => Int -> Tree a -> String
showImpl n Empty = nTabs n ++ "[_]\n"
showImpl n (Node x Empty Empty) = nTabs n ++ "[" ++ show x ++ "]\n"
showImpl n (Node x le ri) =
    showImpl (n + 1) le ++ nTabs n ++ "[" ++ show x ++ "]\n" ++ showImpl (n + 1) ri


nTabs :: Int -> [Char]
nTabs n = replicate n '\t'
