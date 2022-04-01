{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BangPatterns #-}

-- TODO: Insertion does not work properly when
--       a subset gets inserted after. (superset
--       gets erased)

module Data.Trie.Strict (Trie,
    -- Construction
    empty, singleton, fromList, fromFold,
    -- Insertion
    push, insert, insertWith,
    -- Query
    lookup, search, withPrefix, keysWithPrefix,
    (??),
    -- Deletion/Updating
    delete, adjust, update,
    -- Combination
    union, difference, intersect,
    -- Other
    isEmpty, toList, size, genSize, isMemberOf,
    compress,
    -- Pretty
    prettyTrie, printTrie,
) where

import Prelude hiding (lookup)

import Data.Array
import Data.Maybe (isNothing)



type Children a = Array Char (Trie a)


data Trie a
    = Empty
    | Link {
        trieChildren :: !(Children a)
    }
    | Node {
        trieChildren :: !(Children a),
        trieValue :: !a
    }
    deriving (Show, Eq)



{- %%%%%%%%%% Construction %%%%%%%%%% -}

empty :: Trie a
empty = Empty


fromList :: [(String, a)] -> Trie a
fromList = go -- reverse to preserve insertion order
    where     -- being the same as the list order???
        go [] = empty
        go [(!str, !a)] = singleton str a
        go ((!str, !a):rest) = insert str a $! go rest


toList :: Trie a -> [(String, a)]
toList trie = case trie of
    Empty -> []
    Link chs -> map' "" chs
    Node chs a -> (("", a):map' "" chs)
    where
        map' !done = concatMap (go done) . assocs
        go !done (kc, trie') = let key = done ++ [kc] in 
            case trie' of
                Empty -> []
                Link chs -> map' key chs
                Node chs a -> ((key, a):map' key chs)


fromFold :: (Foldable t) => t (String, a) -> Trie a
fromFold = foldr (\(!str, !a) !build ->
    insert str a build) Empty


singleton :: String -> a -> Trie a
singleton [] a = node a
singleton (c:cs) a = Link $!
    newChildren // [(c, singleton cs a)]


-- |Replaces existing values
push :: String -> a -> Trie a -> Trie a
push = insertWith (flip const)


-- |Does not replace an existing value
insert :: String -> a -> Trie a -> Trie a
insert = insertWith const

-- |The first argument to the function given to
-- `insertWith` is the original value
insertWith :: (a -> a -> a) -> String -> a -> Trie a
           -> Trie a
insertWith _ [] _ trie = trie
insertWith _ str a Empty = singleton str a
insertWith f [c] a1 trie = case getChildAt c trie of
    Empty -> setChildAt c (node a1) trie
    Link chs ->
        setChildAt c (Node chs a1) trie
    Node chs a2 -> let !a = f a1 a2 in
        setChildAt c (Node chs a) trie
insertWith f (c:cs) a trie = updateChildAt c
    (insertWith f cs a) trie


{- %%%%%%%%%% Query %%%%%%%%%% -}

search :: String -> Trie a -> Trie a
search [] = id
search (c:cs) = search cs . getChildAt c


lookup :: String -> Trie a -> Maybe a
lookup _ Empty = Nothing
lookup [] (Node _ a) = Just a
lookup [] _ = Nothing
lookup (c:cs) trie' = lookup cs $! getChildAt c trie'


withPrefix :: String -> Trie a -> [(String, a)]
withPrefix pref = go pref . search pref
    where
        -- `done` is the full key so far
        go :: String -> Trie a -> [(String, a)]
        go _ Empty = []
        go done (Link chs) = foldr (\(c, trie) ->
            (go (done ++ [c]) trie ++)
            ) [] $! assocs chs
        go done (Node chs a) = foldr (\(c, trie) ->
            (go (done ++ [c]) trie ++)
            ) [(done, a)] $! assocs chs


keysWithPrefix :: String -> Trie a -> [String]
keysWithPrefix pref trie = let !trie' = search pref trie
    in fmap (pref ++) $! keys trie'


infixl 9 ??
(??) :: Trie a -> String -> Maybe a
(??) = flip lookup


{- %%%%%%%%%% Deletion/Updating %%%%%%%%%% -}

delete :: String -> Trie a -> Trie a
delete [] trie = trie
delete [_] (Node _ _) = Empty
delete [_] trie = trie
delete (c:cs) trie = updateChildAt c (delete cs) trie


adjust :: String -> (a -> a) -> Trie a -> Trie a
adjust _ _ Empty = Empty
adjust [] _ trie = trie
adjust [_] f trie@(Node _ a) = let !a' = f a in
    trie { trieValue = a' }
-- adjust [_] _ trie = trie
adjust (c:cs) f trie = updateChildAt c (adjust cs f) trie


update :: String -> (a -> Maybe a) -> Trie a -> Trie a
update _ _ Empty = Empty
update [] _ trie = trie
update [_] f (Node chs a) = let !a' = f a in case a' of
    Nothing -> Link chs
    Just a'' -> Node chs a''
-- update [_] _ trie = trie
update (c:cs) f trie = updateChildAt c (update cs f) trie


{- %%%%%%%%%% Combining %%%%%%%%%% -}

union :: Trie a -> Trie a -> Trie a
union Empty trie = trie
union trie Empty = trie
union (Node chs a) trie =
    let !chs' = zipChildren difference chs
            (trieChildren trie)
    in Node chs' a
union trie (Node chs a) =
    let !chs' = zipChildren difference
            (trieChildren trie) chs
    in Node chs' a
union tr1 tr2 = Link $! zipChildren union
    (trieChildren tr1) (trieChildren tr2)


difference :: Trie a -> Trie a -> Trie a
difference Empty trie = trie
difference trie Empty = trie
difference (Node chs1 a) (Link chs2) =
    let !chs = zipChildren difference chs1 chs2 in Node chs a
difference (Link chs1) (Node chs2 a) =
    let !chs = zipChildren difference chs1 chs2 in Node chs a
difference tr1 tr2 = Link $! zipChildren difference
    (trieChildren tr1) (trieChildren tr2)


intersect :: Trie a -> Trie a -> Trie a
intersect Empty _ = Empty
intersect _ Empty = Empty
intersect (Node chs1 a) (Node chs2 _) =
    let chs = zipChildren intersect chs1 chs2 in Node chs a
intersect tr1 tr2 = Link $! zipChildren intersect
    (trieChildren tr1) (trieChildren tr2)


{- %%%%%%%%%% Other %%%%%%%%%% -}

keys :: Trie a -> [String]
keys Empty = []
keys trie = foldr ((++) . keys) [] (trieChildren trie)


size :: Trie a -> Int
size Empty = 0 :: Int
size (Link chs) = foldr (\a !b ->
    b + size a) (0 :: Int) chs
size (Node chs _) = foldr (\a !b ->
    b + size a) (1 :: Int) chs


genSize :: (Num n) => Trie a -> n
genSize Empty = 0
genSize (Link chs) = foldr (\a !b ->
    b + genSize a) 0 chs
genSize (Node chs _) = foldr (\a !b ->
    b + genSize a) 1 chs


isEmpty :: Trie a -> Bool
isEmpty Empty = True
isEmpty (Link chs) = all isEmpty chs
isEmpty (Node _ _) = False


isMemberOf :: String -> Trie a -> Bool
isMemberOf _ Empty = False
isMemberOf [] (Node _ _) = True
isMemberOf [] _ = False
isMemberOf str trie
    | isEmpty trie = False
    | otherwise = isNothing (lookup str trie)


compress :: Trie a -> Trie a
compress Empty = Empty
compress trie = trie { trieChildren =
    compressChildren $! trieChildren trie }


{- %%%%%%%%%% Pretty %%%%%%%%%% -}

prettyTrie :: (Show a) => Trie a -> String
prettyTrie = go (0 :: Int) . compress
    where
        idt i = replicate (i * 4 :: Int) ' '
        go i Empty = idt i ++ "Empty"
        go i (Link chs) = idt i ++ "Link:"
            ++ prettyChildren i chs
        go i (Node chs a) = idt i ++ "Node: " ++ show a
            ++ prettyChildren i chs
        prettyChildren i chs = concat
            [if isEmpty trie then "" else
                ('\n':idt i) ++ show c ++ " : " ++
                 -- get rid of indentation
                drop (succ i * 4) (go (i + 1) trie)
             | (c, trie) <- assocs chs]


printTrie :: (Show a) => Trie a -> IO ()
printTrie = putStrLn . prettyTrie



{- %%%%%%%%%%%%%%%%%%%% INTERNALS %%%%%%%%%%%%%%%%%%%% -}


node :: a -> Trie a
node = Node newChildren


compressChildren :: Children a -> Children a
compressChildren = fmap $ \a -> case a of
    Empty -> Empty
    Link chs -> let chs' = compressChildren chs in
        if all isEmpty chs' then Empty else a
    Node chs _ -> a
        { trieChildren = compressChildren chs }


newChildren :: Children a
newChildren = let linkBounds = (minBound, maxBound)
    in array (minBound, maxBound)
        [(k, Empty) | k <- range linkBounds]


oneChild :: Char -> a -> Children a
oneChild !k !a = newChildren // [(k, node a)]


getNonEmpties :: Trie a -> [Trie a]
getNonEmpties Empty = []
getNonEmpties trie = filter (not . isEmpty)
    (elems $! trieChildren trie)


zipChildren :: (Trie a -> Trie a -> Trie a)
            -> Children a -> Children a -> Children a
zipChildren f chs1 chs2 = array (minBound, maxBound)
    [(k, f c1 c2) |
        k <- indices chs1,
        c1 <- elems chs1,
        c2 <- elems chs2]


getChildAt :: Char -> Trie a -> Trie a
getChildAt _ Empty = Empty
getChildAt k trie = trieChildren trie ! k


setChildAt :: Char -> Trie a -> Trie a -> Trie a
setChildAt _ _ Empty = Empty
setChildAt k child trie = trie {
        trieChildren = trieChildren trie // [(k, child)]
    }


updateChildAt :: Char -> (Trie a -> Trie a) -> Trie a
              -> Trie a
updateChildAt k f trie = let !child = getChildAt k trie
    in setChildAt k (f child) trie


-- |Creates a `Key` from a string
-- mkKey :: String -> Key
-- mkKey [] = undefined
-- mkKey (c:cs) = Key c chs


-- |Converts a `Key` back into a string
-- unKey :: Key -> String
-- unKey (Key ch rest) = (ch:rest)



-- instance Show a => Show (Trie a) where
--     show Empty = "empty"
--     show (Link chs) = "fromList" ++ show (assocs chs)
--     show (Node chs a) = "fromList [" ++ dropEnd 2 (
--         foldr (\(k,t) b -> let key = unKey k in
--             if isEmpty t then b else b ++
--                 "("++show key++", "++show t++"), "
--         )) ++ "]"

instance Ord a => Ord (Trie a) where
    compare Empty Empty = EQ
    compare Empty _ = LT
    compare (Link _) Empty = GT
    compare (Link chs1) (Link chs2) = compare chs1 chs2
    compare (Link chs) n@(Node _ _) = foldr (\a' b -> 
        let cmp = compare a' n in case (b, cmp) of
            -- TODO: how to resolve dichotimous conflict?
            (LT, GT) -> EQ -- ???
            (GT, LT) -> EQ -- ???
            _ -> cmp
            ) EQ chs
    compare (Node _ _) Empty = GT
    compare n@(Node _ _) l@(Link _) = compare l n
    compare (Node _ a1) (Node _ a2) = compare a1 a2

instance Functor Trie where
    fmap _ Empty = Empty
    fmap f (Link chs) = let chs' = fmap f <$> chs in
        Link chs'
    fmap f (Node chs a) = Node chs' a'
        where
            !chs' = fmap f <$> chs
            !a' = f a

instance Foldable Trie where
    foldr _ b Empty = b
    foldr f b (Link chs) = foldr (\child !b' ->
        case child of
            Empty -> b'
            Link chs' -> foldr (\child' !b'' ->
                foldr f b'' child') b' chs'
            Node chs' a -> f a $!
                foldr (\child' !b'' ->
                    foldr f b'' child') b' chs'
        ) b chs
    foldr f b (Node chs a) = f a $! foldr
        (\child !b' -> foldr f b' child) b chs
    null = isEmpty


instance Semigroup (Trie a) where
    (<>) = union


instance Monoid (Trie a) where
    mempty = empty
