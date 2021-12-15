module Structs.Tree (
    Tree(Empty, Node),
    leaf,
    insert,
    search,
    toString,
    (<~),
    (<~|)
) where


data Tree a = Empty
    | Node a (Tree a) (Tree a)
    deriving (Show, Eq, Ord)


leaf :: a -> Tree a
leaf x = Node x Empty Empty


insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty x = Node x Empty Empty
insert (Node v Empty Empty) x = Node v (Node x Empty Empty) Empty
insert (Node v l Empty) x = Node v l (Node x Empty Empty)
insert (Node v l r) x
    | (x == v) = Node v l r
    | (x < v)  = Node v (insert l x) r
    | (x > v)  = Node v l (insert r x)


infixl 7 <~
(<~) :: (Ord a) => Tree a -> a -> Tree a
tree <~ x = insert tree x


infixl 6 <~|
(<~|) :: (Ord a) => Tree a -> [a] -> Tree a
tree <~| [] = tree
tree <~| (x:[]) = insert tree x
tree <~| (x:xs) = insert (tree <~| xs) x


search :: (Ord a) => Tree a -> a -> Tree a
search Empty x = Empty
search (Node v l r) x
    | (x == v) = Node v l r
    | (x < v)  = search l x
    | (x > v)  = search r x


toString :: (Show a) => Tree a -> String
toString tree = toStringImpl tree 0


{- IMPLIMENTATION DETAILS -}


toStringImpl :: (Show a) => Tree a -> Int -> String
toStringImpl Empty n = nTabs n ++ "[_]\n"
toStringImpl (Node v Empty Empty) n = nTabs n ++ "[" ++ show v ++ "]\n"
toStringImpl (Node v l r) n =
    toStringImpl l (n + 1) ++ nTabs n ++ "[" ++ show v ++ "]\n" ++ toStringImpl r (n + 1)


nTabs :: Int -> [Char]
nTabs n = replicate n '\t'
