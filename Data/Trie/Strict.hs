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
import Data.Char (ord)
import Data.Maybe (isNothing)
import Text.Printf (printf)



-- BECAUSE I KEEP FORGETTING: the `Key` type is
-- necessary bc the keys for `Children` need to
-- be instances of `Ix`
data Key = Key {
        keyChar :: !Char,
        keyRest :: [Char]
    }
    deriving (Show, Eq, Ord)

instance Bounded Key where
    -- |First printable character (space)
    minBound = Key (toEnum 0x20) []
    -- |Last printable character (tilde (~))
    maxBound = Key (toEnum 0x7e) []

instance Ix Key where
    range (lo, hi) = [Key ch [] |
        ch <- [keyChar lo.. keyChar hi]]
    index (lo, hi) k
        | inRange (lo, hi) k =
            ord (keyChar k) - ord (keyChar lo)
        | otherwise = error "`Key` out of range"
    inRange (lo, hi) k = lo <= k && k <= hi


type Children a = Array Key (Trie a)


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
        go ((str, !a):rest) = insert str a $! go rest


toList :: Trie a -> [(String, a)]
toList trie = case trie of
    Empty -> []
    Link children -> map' "" children
    Node children a -> (("", a):map' "" children)
    where
        map' done cs = concatMap (go done) (assocs cs)
        go done (k, trie') = case trie' of
            Empty -> []
            Link cs -> map' key cs
            Node cs a -> ((key, a):map' key cs)
            where
                key = done ++ unKey k


fromFold :: (Foldable t) => t (String, a) -> Trie a
fromFold = foldr (\(!str, !a) !build ->
    insert str a build) Empty


singleton :: String -> a -> Trie a
singleton [] a = node a
singleton (c:cs) a = setChildAt (Key c cs) (node a)
    (Link newChildren)


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
insertWith f [ch] a1 trie = let key = Key ch [] in
    case getChildAt key trie of
        Empty -> setChildAt key (node a1) trie
        Link cs -> a1 `seq`
            setChildAt key (Node cs a1) trie
        Node cs a2 -> let !a = f a1 a2 in
            setChildAt key (Node cs a) trie
insertWith f (c:cs) a trie = updateChildAt
    (Key c cs) (insertWith f cs a) trie


{- %%%%%%%%%% Query %%%%%%%%%% -}

search :: String -> Trie a -> Trie a
search [] = id
search (c:cs) = search cs . getChildAt (Key c cs)


lookup :: String -> Trie a -> Maybe a
lookup [] (Node _ a) = Just a
lookup [] _ = Nothing
lookup (c:cs) trie = go (Just (Key c cs)) trie
    where
        go :: Maybe Key -> Trie a -> Maybe a
        go _ Empty = Nothing
        go Nothing (Node _ a) = Just a
        go Nothing _ = Nothing
        go (Just key) trie' = go (case keyRest key of
            [] -> Nothing
            (kc:rest) -> Just $ Key kc rest
            )$! (getChildAt key trie')


withPrefix :: String -> Trie a -> [(String, a)]
withPrefix pref = go pref . search pref
    where
        -- `done` is the full key so far
        go :: String -> Trie a -> [(String, a)]
        go _ Empty = []
        go done (Link cs) = foldr (\(key, trie) ->
            (go (done ++ unKey key) trie ++))
            [] (assocs cs)
        go done (Node cs a) = foldr (\(key, trie) ->
            (go (done ++ unKey key) trie ++))
            [(done, a)] (assocs cs)


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
delete (c:cs) trie = let key = Key c cs in
    updateChildAt key (delete cs) trie


adjust :: String -> (a -> a) -> Trie a -> Trie a
adjust _ _ Empty = Empty
adjust [] _ trie = trie
adjust [_] f trie@(Node _ a) = let !a' = f a in
    trie { trieValue = a' }
-- adjust [_] _ trie = trie
adjust (c:cs) f trie = let key = Key c cs in
    updateChildAt key (adjust cs f) trie


update :: String -> (a -> Maybe a) -> Trie a -> Trie a
update _ _ Empty = Empty
update [] _ trie = trie
update [_] f (Node cs a) = let !a' = f a in case a' of
    Nothing -> Link cs
    Just a'' -> Node cs a''
-- adjust [_] _ trie = trie
update (c:cs) f trie = let key = Key c cs in
    updateChildAt key (update cs f) trie


{- %%%%%%%%%% Combining %%%%%%%%%% -}

union :: Trie a -> Trie a -> Trie a
union Empty trie = trie
union trie Empty = trie
union (Node cs a) trie =
    let !cs' = zipChildren difference cs
            (trieChildren trie)
    in Node cs' a
union trie (Node cs a) =
    let !cs' = zipChildren difference
            (trieChildren trie) cs
    in Node cs' a
union tr1 tr2 = Link $! zipChildren union
    (trieChildren tr1) (trieChildren tr2)


difference :: Trie a -> Trie a -> Trie a
difference Empty trie = trie
difference trie Empty = trie
difference (Node cs1 a) (Link cs2) =
    let !cs = zipChildren difference cs1 cs2 in Node cs a
difference (Link cs1) (Node cs2 a) =
    let !cs = zipChildren difference cs1 cs2 in Node cs a
difference tr1 tr2 = Link $! zipChildren difference
    (trieChildren tr1) (trieChildren tr2)


intersect :: Trie a -> Trie a -> Trie a
intersect Empty _ = Empty
intersect _ Empty = Empty
intersect (Node cs1 a) (Node cs2 _) =
    let cs = zipChildren intersect cs1 cs2 in Node cs a
intersect tr1 tr2 = Link $! zipChildren intersect
    (trieChildren tr1) (trieChildren tr2)


{- %%%%%%%%%% Other %%%%%%%%%% -}

keys :: Trie a -> [String]
keys Empty = []
keys trie = foldr ((++) . keys) [] (trieChildren trie)


size :: Trie a -> Int
size Empty = 0 :: Int
size (Link cs) = foldr (\a !b ->
    b + size a) (0 :: Int) cs
size (Node cs _) = foldr (\a !b ->
    b + size a) (1 :: Int) cs


genSize :: (Num n) => Trie a -> n
genSize Empty = 0
genSize (Link cs) = foldr (\a !b ->
    b + genSize a) 0 cs
genSize (Node cs _) = foldr (\a !b ->
    b + genSize a) 1 cs


isEmpty :: Trie a -> Bool
isEmpty Empty = True
isEmpty (Link cs) = all isEmpty cs
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
        go i (Link cs) = idt i ++ "Link:"
            ++ prettyChildren i cs
        go i (Node cs a) = idt i ++ "Node: " ++ show a
            ++ prettyChildren i cs
        prettyChildren i cs = concat
            [if isEmpty trie then "" else ('\n':idt i) ++
                printf "'%c%s' : " ch
                    (if null rest then "" else ('+':rest)) ++
                    (drop (succ i * 4) $! go (i + 1) trie)
                    -- use drop to get rid of indentation ^
             | (Key ch rest, trie) <- assocs cs]


printTrie :: (Show a) => Trie a -> IO ()
printTrie = putStrLn . prettyTrie



{- %%%%%%%%%%%%%%%%%%%% INTERNALS %%%%%%%%%%%%%%%%%%%% -}


node :: a -> Trie a
node = Node newChildren


compressChildren :: Children a -> Children a
compressChildren = fmap $ \a -> case a of
    Empty -> Empty
    Link cs -> let cs' = compressChildren cs in
        if all isEmpty cs' then Empty else a
    Node cs _ -> a
        { trieChildren = compressChildren cs }


newChildren :: Children a
newChildren = let linkBounds = (minBound, maxBound)
    in array (minBound, maxBound)
        [(k, Empty) | k <- range linkBounds]


newChildrenWithAssocs :: [(Key, Trie a)] -> Children a
newChildrenWithAssocs = (//) newChildren


oneChild :: Key -> a -> Children a
oneChild !k !a = newChildrenWithAssocs [(k, node a)]


getNonEmpties :: Trie a -> [(Key, Trie a)]
getNonEmpties Empty = []
getNonEmpties trie = filter (not . (isEmpty . snd))
    (assocs $! trieChildren trie)


zipChildren :: (Trie a -> Trie a -> Trie a)
            -> Children a -> Children a -> Children a
zipChildren f cs1 cs2 = array (bounds cs1)
    [(k, f c1 c2) |
        k <- indices cs1,
        c1 <- elems cs1,
        c2 <- elems cs2]


insertChild :: Key -> a -> Children a -> Children a
insertChild !k !a !cs = cs // [(k, node a)]


getChildAt :: Key -> Trie a -> Trie a
getChildAt _ Empty = Empty
getChildAt k trie = trieChildren trie ! k


setChildAt :: Key -> Trie a -> Trie a -> Trie a
setChildAt _ _ Empty = Empty
setChildAt k child trie = trie {
        trieChildren = trieChildren trie // [(k, child)]
    }


updateChildAt :: Key -> (Trie a -> Trie a) -> Trie a
              -> Trie a
updateChildAt k f trie = setChildAt k (f child) trie
    where
        child = getChildAt k trie


-- |Creates a `Key` from a string
-- mkKey :: String -> Key
-- mkKey [] = undefined
-- mkKey (c:cs) = Key c cs


-- |Converts a `Key` back into a string
unKey :: Key -> String
unKey (Key ch rest) = (ch:rest)



-- instance Show a => Show (Trie a) where
--     show Empty = "empty"
--     show (Link cs) = "fromList" ++ show (assocs cs)
--     show (Node cs a) = "fromList [" ++ dropEnd 2 (
--         foldr (\(k,t) b -> let key = unKey k in
--             if isEmpty t then b else b ++
--                 "("++show key++", "++show t++"), "
--         )) ++ "]"

instance Ord a => Ord (Trie a) where
    compare Empty Empty = EQ
    compare Empty _ = LT
    compare (Link _) Empty = GT
    compare (Link cs1) (Link cs2) = compare cs1 cs2
    compare (Link cs) n@(Node _ _) = foldr (\a' b -> 
        let cmp = compare a' n in case (b, cmp) of
            -- TODO: how to resolve dichotimous conflict?
            (LT, GT) -> EQ -- ???
            (GT, LT) -> EQ -- ???
            _ -> cmp
            ) EQ cs
    compare (Node _ _) Empty = GT
    compare n@(Node _ _) l@(Link _) = compare l n
    compare (Node _ a1) (Node _ a2) = compare a1 a2

instance Functor Trie where
    fmap _ Empty = Empty
    fmap f (Link cs) = let cs' = fmap f <$> cs in
        Link cs'
    fmap f (Node cs a) = Node cs' a'
        where
            !cs' = fmap f <$> cs
            !a' = f a

instance Foldable Trie where
    foldr _ b Empty = b
    foldr f b (Link cs) = foldr (\child !b' ->
        case child of
            Empty -> b'
            Link cs' -> foldr (\child' !b'' ->
                foldr f b'' child') b' cs'
            Node cs' a -> f a $!
                foldr (\child' !b'' ->
                    foldr f b'' child') b' cs'
        ) b cs
    foldr f b (Node cs a) = f a $! foldr
        (\child !b' -> foldr f b' child) b cs
    null = isEmpty


instance Semigroup (Trie a) where
    (<>) = union


instance Monoid (Trie a) where
    mempty = empty
