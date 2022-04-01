module Data.AdvTree where

import Text.Printf (printf)

import Prelude hiding (Either(..))



data TreeType
    = Left | Root | Right
    deriving (Show, Eq, Ord)


data Tree a
    = Empty {
        cmpFn  :: (a -> a -> Ordering),
        ttype  :: TreeType
    }
    | Node {
        value  :: a,
        left   :: !(Tree a),
        right  :: !(Tree a),
        dupe_c :: Int,
        cmpFn  :: !(a -> a -> Ordering),
        ttype  :: TreeType
    }



empty :: (a -> a -> Ordering) -> Tree a
empty cmp = Empty cmp Root


new :: (a -> a -> Ordering) -> a -> Tree a
new cmp a = Node {
        value = a,
        left = (Empty cmp Left),
        right = (Empty cmp Right),
        dupe_c = 0,
        cmpFn = cmp,
        ttype = Root
    }


basic :: (Ord a) => Tree a
basic = Empty compare Root


newBasic :: (Ord a) => a -> Tree a
newBasic a = new compare a 


selfCmp :: Tree a -> a -> Ordering
selfCmp (Empty _ _) _ = error "selfCmp Empty _"
selfCmp n x = (cmpFn n) (value n) x


treeCmp :: Tree a -> Tree a -> Ordering
treeCmp (Node lv _ _ _ cmp _) (Node rv _ _ _ _ _) = cmp lv rv
treeCmp _ _ = error "treeCmp"


swap :: Tree a -> Tree a -> (Tree a, Tree a)
swap le@(Node lv _ _ ld _ _) ri@(Node rv _ _ rd _ _) =
    (le', ri')
    where
        le' = le {
            value = rv,
            dupe_c = rd
        }
        ri' = ri {
            value = lv,
            dupe_c = ld
        }
swap le ri = (le, ri)


-- |O(n)
invert :: Tree a -> Tree a
invert t@(Empty _ _) = t
invert tree@(Node _ l@(Empty _ _) r@(Empty _ _) _ _ _) = tree
invert tree@(Node _ l r _ _ _) = tree {
        left = invert $ r { ttype = Left },
        right = invert $ l { ttype = Right }
    }


-- |O(log n)
insert :: a -> Tree a -> Tree a
insert a (Empty cmp _) = new cmp a
insert a n@(Node v l r d cmp _) = case cmp a v of
    LT -> n { left = (insert a l) { ttype = Left } }
    EQ -> n { dupe_c = d + 1 }
    GT -> n { right = (insert a r) { ttype = Right } }


-- |O(log n)
insertNode :: Tree a -> Tree a -> Tree a
insertNode (Empty _ _) t = t
insertNode n (Empty _ _) = n
insertNode n t@(Node _ l r d _ _) = case treeCmp n t of
    LT -> t { left = (insertNode n l) { ttype = Left } }
    EQ -> t { dupe_c = d + dupe_c n }
    GT -> t { right = (insertNode n r) { ttype = Right } }


-- |O(log n)
search :: a -> Tree a -> Tree a
search _ (Empty cmp t) = Empty cmp t
search a n@(Node v l r _ cmp _) = case cmp a v of
    LT -> search a l
    EQ -> n
    GT -> search a r


-- |O(log n)
extract:: a -> Tree a -> Tree a
extract _ (Empty cmp t) = Empty cmp t
extract a n@(Node v l r _ cmp _) = case cmp a v of
    LT -> n { left = extract a l }
    EQ -> delete n
    GT -> n { right = extract a r }


-- |O(1)
delete :: Tree a -> Tree a
delete (Empty cmp t) = Empty cmp t
delete (Node _ l r 0 cmp t) = case l of
    Empty _ _ -> r { ttype = t }
    Node _ _ _ _ _ _ -> case r of
        Empty _ _ -> l { ttype = t }
        Node rv rl rr rd _ _ -> case rl of
            Empty _ _ -> Node rv l rr rd cmp t
            Node rlv _ rlr rld _ _
                -> Node rlv l (r { left = rlr }) rld cmp t
delete n@(Node _ _ _ d _ _) = n { dupe_c = d - 1 }


minHeapify :: Tree a -> Tree a
minHeapify t@(Empty _ _) = t
minHeapify tree = sift (cfmapt minHeapify tree)
    where
        sift tree'@(Node x le@(Node lx _ _ _ _ _) (Empty _ _) _ _ _)
            | (lte x lx) = tree' { left = sift le }
            | (otherwise) = tree'' { left = sift le' }
            where
                (tree'', le') = swap tree' le
        sift tree'@(Node _ (Empty _ _) ri _ _ _)
            = sift $ tree' {
                left = ri { ttype = Left },
                right = Empty cmp Right
            }
        sift tree'@(Node x le@(Node lx _ _ _ _ _) ri@(Node rx _ _ _ _ _) _ _ _)
            | (lte x lx && lte x rx) = tree'
            | (lte lx x && lte lx rx)
                = lsTree { left = sift le' }
            | (lte rx x && lte rx lx)
                = rsTree { right = sift ri' }
            where
                (lsTree, le') = swap tree' le
                (rsTree, ri') = swap tree' ri
        sift t = t
        cmp = cmpFn tree
        lte a b = let res = cmp a b
            in res == LT || res == EQ


maxHeapify :: Tree a -> Tree a
maxHeapify t@(Empty _ _) = t
maxHeapify tree = sift (cfmapt maxHeapify tree)
    where
        sift tree'@(Node x le@(Node lx _ _ _ _ _) (Empty _ _) _ _ _)
            | (gte x lx) = tree' { left = sift le }
            | (otherwise) = let (tree'', le') = swap tree' le
                in tree'' { left = sift le' }
        sift tree'@(Node _ (Empty _ _) ri _ _ _)
            = sift $ tree' {
                left = ri { ttype = Left },
                right = Empty cmp Right
            }
        sift tree'@(Node x le@(Node lx _ _ _ _ _) ri@(Node rx _ _ _ _ _) _ _ _)
            | (gte x lx && gte x rx) = tree'
            | (gte lx x && gte lx rx)
                = let (tree'', le') = swap tree' le
                    in tree'' { left = sift le' }
            | (gte rx x && gte rx lx)
                = let (tree'', ri') = swap tree' ri
                    in tree'' { right = sift ri' }
        sift t = t
        cmp = cmpFn tree
        gte a b = let res = cmp a b
            in res == GT || res == EQ


cfmapt :: (Tree a -> Tree a) -> Tree a -> Tree a
cfmapt f tree = tree {
        left = f $ left tree,
        right = f $ right tree
    }


-- |the height of a tree is the greatest number of edges
-- in a path from one node to its lowest child
height :: (Integral n) => Tree a -> n
height (Empty _ _) = 0
height (Node _ (Empty _ _) (Empty _ _) _ _ _) = 0
height (Node _ l r _ _ _) = 1 + max (height l) (height r)


-- |@'expanse' t@ will give the number of nodes in a tree,
-- which does not count duplicates
expanse :: (Integral n) => Tree a -> n
expanse (Empty _ _) = 0
expanse (Node _ l r _ _ _) = 1 + expanse l + expanse r


-- |@'size' t@ will give the number of elements in a tree,
-- counting duplicates
size :: (Integral n) => Tree a -> n
size (Empty _ _) = 0
size (Node _ l r d _ _) = size l + size r + fromIntegral d


(=!=) :: (Eq a) => Tree a -> Tree a -> Bool
    Empty _ lt == Empty _ rt = lt == rt
    Node lv ll lr ld _ lt == Node rv rl rr rd _ rt =
        lv == rv && ll =!= rl && lr =!= rr && ld == rd && lt == rt


(===) :: (Eq a) => Tree a -> Tree a -> Bool
    Empty _ _ === Empty _ _ = True
    Node lv ll lr _ _ _ === Node rv rl rr _ _ _ =
        lv == rv && ll === rl && lr === rr


pretty :: (Show a) => Tree a -> String
pretty (Empty _ t) = printf "Empty _ %s\n" (show t)
pretty t = pretty' t 0
    where
        tabs n = replicate n '\t'
        pretty' (Empty _ tt) n
            = printf "%sEmpty _ %s"
                (tabs n)
                (show tt)
        pretty' (Node v l r d _ _) n
            = printf "%s\n%sNode {%s (%d)}\n%s"
                (pretty' l (n + 1)) (tabs n)
                (show v) d
                (pretty' r (n + 1))


instance Foldable Tree where
    foldr _ b (Empty _ _) = b
    foldr f b (Node v l r d _ _) = foldr f (f' d (foldr f b l)) r
        where
            f' n x
                | (n <= 0)    = f v x
                | (otherwise) = f' (n - 1) (f v x)


instance Functor Tree where
    fmap _ tree@(Empty _ _) = tree
    fmap f tree@(Node v l r _ _ _) = tree {
            value = f v,
            left = fmap f l,
            right = fmap f r
        }


instance (Eq a) => Eq (Tree a) where
    Empty _ lt == Empty _ rt = lt == rt
    Node lv ll lr ld _ lt == Node rv rl rr rd _ rt
        = lv == rv && ll == rl && lr == rr && ld == rd && lt == rt
    _ == _ = False
