{-# LANGUAGE BangPatterns #-}

module Data.Trie.Internal where

import Prelude hiding (lookup)

import Data.Array hiding (
    assocs,
    elems
    )
import qualified Data.Array as A
import Data.Maybe (isNothing, fromMaybe)
import Data.Semigroup

default (Int)



type Children a = Array Char (Trie a)


data Trie a
    = Empty
    | Link {
        -- array with indices that represent the
        -- character following `trieCommon`
        trieChildren :: !(Children a),
        -- represents characters that are common
        -- amongst the children
        trieCommon :: String
    }
    | Node {
        trieChildren :: !(Children a),
        trieValue :: a
    }
    deriving (Show, Eq)



{- %%%%%%%%%% Construction %%%%%%%%%% -}

-- |Creates an empty trie
empty :: Trie a
{-# INLINE empty #-}
empty = Empty


-- |Creates a trie from a list of key-value pairs
fromList :: [(String, a)] -> Trie a
{-# INLINE fromList #-}
fromList [] = empty
fromList [(!str, !a)] = singleton str a
fromList ((!str, !a):rest) = insert str a $! fromList rest


-- |Generalization of @`fromList`@ to all @`Foldable`@
-- instances
fromFold :: (Foldable t) => t (String, a) -> Trie a
{-# INLINE fromFold #-}
fromFold = foldr (\(!str, !a) !build ->
    insert str a build) Empty


-- |Creates a single-element trie
singleton :: String -> a -> Trie a
{-# INLINE singleton #-}
singleton [] a = node a
singleton [!c] !a = Link (oneChild c (node a)) []
singleton !str !a = let (cs, c) = (init str, last str)
    in Link (oneChild c (node a)) cs


-- |Inserts a value at the given key. If a value at
-- that key already exists, it will be replaced.
push :: String -> a -> Trie a -> Trie a
{-# INLINE push #-}
push = insertWith (flip const)


-- |Inserts a value at the given key. If a value at
-- that key already exists, nothing will happen.
insert :: String -> a -> Trie a -> Trie a
{-# INLINE insert #-}
insert = insertWith const


-- |@`insertWith` f key val trie@ inserts @val@ at
-- @key@. If a value @old@ at that key already exists,
-- it is replaced with @f old val@.
insertWith :: (a -> a -> a) -> String -> a -> Trie a
           -> Trie a
insertWith _ [] _ !trie = trie
insertWith _ !str !a Empty = singleton str a
insertWith f [c] a1 !trie = setChildAt c newChild trie
    where
        newChild = case getChildAt c trie of
            Empty -> a1 `seq` node a1
            Link chn [] -> a1 `seq` Node chn a1
            Link chn (c':cs) -> a1 `seq`
                Node (oneChild c' (Link chn cs)) a1
            Node chn a2 -> let !a = f a1 a2 in
                Node chn a
insertWith f str@(sc:scs) !a trie@(Link chn com@(cc:ccs)) =
    case keyDiff str com of
        NoPrefix -> Link (newChildren // [
                (sc, singleton scs a),
                (cc, Link chn ccs)
            ]) []
        Equal -> let (cs, c) = (init str, last str) in
            Link (newChildren // [(c, Node chn a)]) cs
        RightEx _ (sh:st) ->
            updateChildAt sh (insertWith f st a) trie
        Diff pref (sh:st) (ch:ct) -> Link (newChildren // [
                (sh, singleton st a),
                (ch, Link chn ct)
            ]) pref
        _ -> error "IMPOSSIBLE"
insertWith f (c:cs) a trie = updateChildAt c
    (insertWith f cs a) trie


{- %%%%%%%%%% Query %%%%%%%%%% -}

-- |Deprecated for users
search :: String -> Trie a -> Trie a
search [] !trie = trie
search str@(c:cs) trie = case trie of
    Empty -> Empty
    Link chn com -> case keyDiff str com of
        RightEx _ (c':cs') -> search cs' $!
            chn ! c'
        Diff _ (c':cs') _ -> search cs' $!
            chn ! c'
        _ -> Empty
    _ -> search cs $! getChildAt c trie


-- |Searches the trie for the given key. Returns
-- @`Just` val@ if found, and @`Nothing`@ otherwise
lookup :: String -> Trie a -> Maybe a
{-# INLINE lookup #-}
lookup str trie = case search str trie of
    Node _ !a -> Just a
    _ -> Nothing


-- |Searches the trie for the given key, returning
-- the default value if not found.
findWithDefault :: a -> String -> Trie a -> a
{-# INLINE findWithDefault #-}
findWithDefault def str = fromMaybe def . lookup str


-- |Operator alias for @`flip` `lookup`@
infixl 9 ??
(??) :: Trie a -> String -> Maybe a
{-# INLINE (??) #-}
(??) = flip lookup


{- %%%%%%%%%% Deletion/Updating %%%%%%%%%% -}

-- |Deletes the value at the given key.
delete :: String -> Trie a -> Trie a
delete [] trie = trie
delete [_] (Node chn _)
    | all isEmpty chn = Empty
    | otherwise = Link chn []
delete [_] trie = trie
delete (c:cs) trie = updateChildAt c (delete cs) trie


-- |Update the value at the given key with the given
-- function
adjust :: (a -> a) -> String -> Trie a -> Trie a
adjust _ _ Empty = Empty
adjust _ [] trie = trie
adjust f [_] trie@(Node _ a) = let !a' = f a in
    trie { trieValue = a' }
-- adjust [_] _ trie = trie
adjust f (c:cs) trie = updateChildAt c (adjust f cs) trie


-- |In the expression @`update` f key trie@, if
-- @f old@ is @`Nothing`@, it the value at @key@ is
-- deleted, but if it is @`Just` val@ then the value
-- at @key@ is @val@0
update :: (a -> Maybe a) -> String -> Trie a -> Trie a
update _ _ Empty = Empty
update _ [] trie = trie
update f [_] (Node chn a) = let !a' = f a in case a' of
    Nothing -> Link chn [] -- effectively delete
    Just a'' -> Node chn a''
-- update [_] _ trie = trie
update f (c:cs) trie = updateChildAt c (update f cs) trie


{- %%%%%%%%%% Combining %%%%%%%%%% -}

-- |Creates a trie consisting of the all items in both tries.
-- Left preferential for items of shared keys.
union :: Trie a -> Trie a -> Trie a
union Empty trie = trie
union trie Empty = trie
union (Node chn a) trie =
    let !chn' = zipChn union chn (trieChildren trie)
    in Node chn' a
union trie (Node chn a) =
    let !chn' = zipChn union (trieChildren trie) chn
    in Node chn' a
union l1@(Link chn1 com1) l2@(Link chn2 com2)
    | all isEmpty chn1 = compress l2
    | all isEmpty chn2 = compress l1
    | otherwise = case keyDiff com1 com2 of
        Equal -> chn `seq` Link chn com1
        NoPrefix -> case (com1, com2) of
            ([], _) -> chn `seq` Link chn com2
            (_, []) -> chn `seq` Link chn com1
            ((c1:cs1), (c2:cs2)) -> Link (newChildren // [
                    (c1, Link chn1 cs1),
                    (c2, Link chn2 cs2)
                ]) []
        LeftEx pref (c:cs) ->
            setChildAt c (Link chn2 cs) (Link chn1 pref)
        RightEx pref (c:cs) ->
            setChildAt c (Link chn1 cs) (Link chn2 pref)
        Diff pref (c1:cs1) (c2:cs2) -> Link
            (newChildren // [
                    (c1, Link chn1 cs1),
                    (c2, Link chn2 cs2)
                ]) pref
        _ -> error "theoretically impossible"
        where
            chn = zipChn union chn1 chn2


-- -- |Creates a trie consisting only of the common items.
-- -- Left preferential.
-- intersect :: Trie a -> Trie a -> Trie a
-- intersect Empty _ = Empty
-- intersect _ Empty = Empty
-- intersect (Node chn1 a) (Node chn2 _) =
--     let chn = zipChn intersect chn1 chn2 in Node chn a
-- intersect (Node chn1 _) (Link chn2 com) =
--     Link (zipChn intersect chn1 chn2) com
-- intersect l@(Link _ _) n@(Node _ _) = intersect n l
-- intersect (Link chn1 com1) (Link chn2 com2) = Link chn com
--     where
--         (com, uncom) = break (curry (==)) $! zip com1 com2
--         chn = if null uncom then
--                 zipChn intersect chn1 chn2
--             else
--                 zipChn (updateChildAt (head uncom)
--                     (intersect))
--                 zipChn intersect chn1 chn2
-- intersect tr1 tr2 = Link (zipChn intersect
--     (trieChildren tr1) (trieChildren tr2))


{- %%%%%%%%%% Other %%%%%%%%%% -}

-- |Returns a list of the key-value pairs
assocs :: Trie a -> [(String, a)]
assocs trie = case trie of
    Empty -> []
    Link !chn com -> map' com chn
    Node !chn !a -> (([], a):map' [] chn)
    where
        map' !done = concatMap (go done) . A.assocs
        go !done (c, trie') = case trie' of
            Empty -> []
            Link !chn com -> map' (done ++ (c:com)) chn
            Node !chn !a -> let key = done ++ [c] in
                ((key, a):map' key chn)


-- |Returns a list of all keys in the trie
keys :: Trie a -> [String]
{-# INLINE keys #-}
keys Empty = []
keys trie = foldr ((++) . keys) [] (trieChildren trie)


-- |Returns a list of all values in the trie
elems :: Trie a -> [a]
{-# INLINE elems #-}
elems Empty = []
elems (Link chn _) = foldr ((++) . elems) [] chn
elems (Node chn a) = foldr ((++) . elems) [a] chn


-- |Returns a list of key-value pairs, where the values
-- are all values whose key starts with the given string
assocsWithPrefix :: String -> Trie a -> [(String, a)]
assocsWithPrefix _ Empty = []
assocsWithPrefix pref (Link !chn com) = concatMap (
    \(c,trie) -> assocsWithPrefix
        (pref ++ com ++ [c]) trie) (A.assocs chn)
assocsWithPrefix pref (Node !chn !a) = (pref, a) :
    concatMap (\(c,trie) -> assocsWithPrefix
        (pref ++ [c]) trie) (A.assocs chn)


-- |Returns a list of all keys that start with the given
-- string
keysWithPrefix :: String -> Trie a -> [String]
keysWithPrefix pref trie = let !trie' = search pref trie
    in fmap (pref ++) $! keys trie'


-- |Returns a list of all values whose key starts with
-- the given string
elemsWithPrefix :: String -> Trie a -> [a]
{-# INLINE elemsWithPrefix #-}
elemsWithPrefix pref = elems . search pref 


-- |The size of the trie
size :: Trie a -> Int
{-# INLINE size #-}
size Empty = 0 :: Int
size (Link chn _) = foldr (\a !b ->
    b + size a) (0 :: Int) chn
size (Node chn _) = foldr (\a !b ->
    b + size a) (1 :: Int) chn


-- |@`size`@ generalized to any @`Num`@ instance
genSize :: (Num n) => Trie a -> n
{-# INLINE genSize #-}
genSize Empty = fromInteger 0
genSize (Link chn _) = foldr (\a !b ->
    b + genSize a) (fromInteger 0) chn
genSize (Node chn _) = foldr (\a !b ->
    b + genSize a) (fromInteger 1) chn


-- |Returns whether or not the trie is empty
isEmpty :: Trie a -> Bool
{-# INLINE isEmpty #-}
isEmpty Empty = True
isEmpty (Link chn _) = all isEmpty chn
isEmpty (Node _ _) = False


-- |Checks if the key exists in the trie
isMemberOf :: String -> Trie a -> Bool
{-# INLINE isMemberOf #-}
isMemberOf str = isNothing . lookup str
-- isMemberOf _ Empty = False
-- isMemberOf [] (Node _ _) = True
-- isMemberOf [] _ = False
-- isMemberOf str@(c:cs) (Link chn com) =
--     case keyDiff str com of
--         Equal -> False
--         NoPrefix -> False
-- isMemberOf str@(c:cs) (Node chn _) =
--     cs `isMemberOf` chn!c


-- |Removes excess trie nodes. Typically not needed.
compress :: Trie a -> Trie a
{-# INLINE compress #-}
compress Empty = Empty
compress trie = trie { trieChildren =
    compressChildren $! trieChildren trie }


{- %%%%%%%%%% Pretty %%%%%%%%%% -}

prettyTrie :: (Show a) => Trie a -> String
prettyTrie = go (0 :: Int) . compress
    where
        idt i = replicate (i * 4 :: Int) ' '
        go i Empty = idt i ++ "Empty"
        go i (Link chn com) = idt i ++ "Link: " ++
            show com ++ prettyChildren i chn
        go i (Node chn a) = idt i ++ "Node: " ++ show a
            ++ prettyChildren i chn
        prettyChildren i chn = concat
            [if isEmpty trie then "" else
                ('\n':idt (i + 1)) ++ show c ++ " : "
                 -- get rid of indentation
                ++ drop (succ i * 4) (go (i + 1) trie)
             | (c, trie) <- A.assocs chn]


printTrie :: (Show a) => Trie a -> IO ()
printTrie = putStrLn . prettyTrie



{- %%%%%%%%%%%%%%%%%%%% INTERNALS %%%%%%%%%%%%%%%%%%%% -}


node :: a -> Trie a
{-# INLINE node #-}
node = Node newChildren


common :: String -> Trie a
{-# INLINE common #-}
common = Link newChildren


link :: Trie a
{-# INLINE link #-}
link = Link newChildren []


compressChildren :: Children a -> Children a
compressChildren = fmap $ \a -> case a of
    Empty -> Empty
    Link chn _ -> let !chn' = compressChildren chn in
        if all isEmpty chn' then Empty else a
    Node chn _ -> a
        { trieChildren = compressChildren chn }


keyBounds :: (Char, Char)
{-# INLINE keyBounds #-}
keyBounds = (toEnum (0x20 :: Int), toEnum (0x7e :: Int))


mkChildren :: [(Char, Trie a)] -> Children a
{-# INLINE mkChildren #-}
mkChildren = array keyBounds


newChildren :: Children a
{-# INLINE newChildren #-}
newChildren = mkChildren [(k, Empty) | k <- range keyBounds]


oneChild :: Char -> Trie a -> Children a
{-# INLINE oneChild #-}
oneChild !k trie = newChildren // [(k, trie)]


getNonEmpties :: Trie a -> [Trie a]
{-# INLINE getNonEmpties #-}
getNonEmpties Empty = []
getNonEmpties trie = filter (not . isEmpty)
    (A.elems $! trieChildren trie)


zipChn :: (Trie a -> Trie a -> Trie a)
            -> Children a -> Children a -> Children a
{-# INLINE zipChn #-}
zipChn f chn1 chn2 = mkChildren [(k, f c1 c2) |
    k <- indices chn1,
    c1 <- A.elems chn1,
    c2 <- A.elems chn2]


getChildAt :: Char -> Trie a -> Trie a
{-# INLINE getChildAt #-}
getChildAt _ Empty = Empty
getChildAt k trie = trieChildren trie ! k


setChildAt :: Char -> Trie a -> Trie a -> Trie a
{-# INLINE setChildAt #-}
setChildAt _ _ Empty = Empty
setChildAt k child trie = trie {
        trieChildren = trieChildren trie // [(k, child)]
    }


updateChildAt :: Char -> (Trie a -> Trie a) -> Trie a
              -> Trie a
{-# INLINE updateChildAt #-}
updateChildAt k f trie = let !child = getChildAt k trie
    in setChildAt k (f child) trie


data KeyDiff
    = Equal
    | NoPrefix
    | LeftEx String String
    | RightEx String String
    | Diff String String String
    deriving (Show, Eq)


keyDiff :: String -> String -> KeyDiff
{-# INLINE keyDiff #-}
keyDiff [] [] = Equal
keyDiff [] str = LeftEx [] str
keyDiff str [] = RightEx [] str 
keyDiff (c1:cs1) (c2:cs2)
    | c1 == c2 = case keyDiff cs1 cs2 of
        Equal -> Equal
        NoPrefix -> Diff [c1] cs1 cs2
        LeftEx ps cs2' -> LeftEx (c2:ps) cs2'
        RightEx ps cs1' -> RightEx (c1:ps) cs1'
        Diff ps cs1' cs2' -> Diff (c1:ps) cs1' cs2'
    | otherwise = NoPrefix


instance Functor Trie where
    fmap _ Empty = Empty
    fmap f (Link !chn com) = let !chn' = fmap f <$> chn in
        Link chn' com
    fmap f (Node !chn !a) = Node chn' a'
        where
            !chn' = fmap f <$> chn
            !a' = f a

instance Foldable Trie where
    foldr _ b Empty = b
    foldr f b (Link chn _) = foldr (\child !b' ->
        case child of
            Empty -> b'
            Link chn' _ -> foldr (\child' !b'' ->
                foldr f b'' child') b' chn'
            Node chn' a -> f a $!
                foldr (\child' !b'' ->
                    foldr f b'' child') b' chn'
        ) b chn
    foldr f b (Node chn a) = f a $! foldr
        (\child !b' -> foldr f b' child) b chn
    null = isEmpty


instance Semigroup (Trie a) where
    (<>) = union
    -- union is idempotent (i.e. trie `union` trie == trie).
    stimes = stimesIdempotentMonoid


instance Monoid (Trie a) where
    mempty = empty
