{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BangPatterns #-}

module Data.Trie.Internal where

import Prelude hiding (lookup)

import Data.Array
import Data.Maybe (isNothing)
-- import Data.List (isPrefixOf)

import Debug.Trace



type Children a = Array Char (Trie a)


data Trie a
    = Empty
    | Link {
        -- array with indices that represent the
        -- character following `trieCommon`
        trieChildren :: !(Children a),
        -- |represents characters that are common
        -- amongst the children
        trieCommon :: String
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
    Link chn com -> map' com chn
    Node chn a -> (([], a):map' [] chn)
    where
        map' !done = concatMap (go done) . assocs
        go !done (c, trie') = case trie' of
            Empty -> []
            Link chn com -> map' (done ++ (c:com)) chn
            Node chn a -> let key = done ++ [c] in
                ((key, a):map' key chn)


fromFold :: (Foldable t) => t (String, a) -> Trie a
fromFold = foldr (\(!str, !a) !build ->
    insert str a build) Empty


singleton :: String -> a -> Trie a
singleton [] a = node a
singleton [c] a = Link (oneChild c (node a)) []
singleton str a = let (cs, c) = (init str, last str)
    in Link (oneChild c (node a)) cs


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
insertWith f [c] a1 trie = setChildAt c newChild trie
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
        LeftEx _ _ -> error "tbd"
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

search :: String -> Trie a -> Trie a
search [] trie = trie
search str@(c:cs) trie = case trie of
    Empty -> Empty
    Link chn com -> case keyDiff str com of
        RightEx _ (c':cs') -> search cs' $!
            chn ! c'
        Diff _ (c':cs') _ -> search cs' $!
            chn ! c'
        _ -> Empty
    _ -> search cs $! getChildAt c trie


lookup :: String -> Trie a -> Maybe a
lookup str trie = case search str trie of
    Node _ a -> Just a
    _ -> Nothing


-- withPrefix :: String -> Trie a -> [(String, a)]
-- withPrefix pref = go pref . search pref
--     where
--         -- `done` is the full key so far
--         go :: String -> Trie a -> [(String, a)]
--         go _ Empty = []
--         go done (Link chn) = foldr (\(c, trie) ->
--             (go (done ++ [c]) trie ++)
--             ) [] $! assocs chn
--         go done (Node chn a) = foldr (\(c, trie) ->
--             (go (done ++ [c]) trie ++)
--             ) [(done, a)] $! assocs chn


-- keysWithPrefix :: String -> Trie a -> [String]
-- keysWithPrefix pref trie = let !trie' = search pref trie
--     in fmap (pref ++) $! keys trie'


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
update [_] f (Node chn a) = let !a' = f a in case a' of
    Nothing -> Link chn [] -- effectively delete
    Just a'' -> Node chn a''
-- update [_] _ trie = trie
update (c:cs) f trie = updateChildAt c (update cs f) trie


{- %%%%%%%%%% Combining %%%%%%%%%% -}

-- -- |Creates a trie consisting of the all items in both tries.
-- -- Left preferential for items of shared keys.
-- union :: Trie a -> Trie a -> Trie a
-- union Empty trie = trie
-- union trie Empty = trie
-- union (Node chn a) trie =
--     let !chn' = zipChildren difference chn
--             (trieChildren trie)
--     in Node chn' a
-- union trie (Node chn a) =
--     let !chn' = zipChildren difference
--             (trieChildren trie) chn
--     in Node chn' a
-- union tr1 tr2 = Link $! zipChildren union
--     (trieChildren tr1) (trieChildren tr2)


-- -- |Creates a trie consisting only of the unshared items.
-- difference :: Trie a -> Trie a -> Trie a
-- difference Empty trie = trie
-- difference trie Empty = trie
-- difference (Node chn1 a) (Link chn2) =
--     let !chn = zipChildren difference chn1 chn2 in Node chn a
-- difference (Link chn1) (Node chn2 a) =
--     let !chn = zipChildren difference chn1 chn2 in Node chn a
-- difference tr1 tr2 = Link $! zipChildren difference
--     (trieChildren tr1) (trieChildren tr2)


-- -- |Creates a trie consisting only of the common items.
-- -- Left preferential.
-- intersect :: Trie a -> Trie a -> Trie a
-- intersect Empty _ = Empty
-- intersect _ Empty = Empty
-- intersect (Node chn1 a) (Node chn2 _) =
--     let chn = zipChildren intersect chn1 chn2 in Node chn a
-- intersect (Node chn1 _) (Link chn2 com) =
--     Link (zipChildren intersect chn1 chn2) com
-- intersect l@(Link _ _) n@(Node _ _) = intersect n l
-- intersect (Link chn1 com1) (Link chn2 com2) = Link chn com
--     where
--         (com, uncom) = break (curry (==)) $! zip com1 com2
--         chn = if null uncom then
--                 zipChildren intersect chn1 chn2
--             else
--                 zipChildren (updateChildAt (head uncom)
--                     (intersect))
--                 zipChildren intersect chn1 chn2
-- intersect tr1 tr2 = Link (zipChildren intersect
--     (trieChildren tr1) (trieChildren tr2))


{- %%%%%%%%%% Other %%%%%%%%%% -}

keys :: Trie a -> [String]
keys Empty = []
keys trie = foldr ((++) . keys) [] (trieChildren trie)


size :: Trie a -> Int
size Empty = 0 :: Int
size (Link chn _) = foldr (\a !b ->
    b + size a) (0 :: Int) chn
size (Node chn _) = foldr (\a !b ->
    b + size a) (1 :: Int) chn


genSize :: (Num n) => Trie a -> n
genSize Empty = 0
genSize (Link chn _) = foldr (\a !b ->
    b + genSize a) 0 chn
genSize (Node chn _) = foldr (\a !b ->
    b + genSize a) 1 chn


isEmpty :: Trie a -> Bool
isEmpty Empty = True
isEmpty (Link chn _) = all isEmpty chn
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
        go i (Link chn com) = idt i ++ "Link: " ++
            show com ++ prettyChildren i chn
        go i (Node chn a) = idt i ++ "Node: " ++ show a
            ++ prettyChildren i chn
        prettyChildren i chn = concat
            [if isEmpty trie then "" else
                ('\n':idt (i + 1)) ++ show c ++ " : "
                 -- get rid of indentation
                ++ drop (succ i * 4) (go (i + 1) trie)
             | (c, trie) <- assocs chn]


printTrie :: (Show a) => Trie a -> IO ()
printTrie = putStrLn . prettyTrie



{- %%%%%%%%%%%%%%%%%%%% INTERNALS %%%%%%%%%%%%%%%%%%%% -}


node :: a -> Trie a
node = Node newChildren


common :: String -> Trie a
common = Link newChildren


link :: Trie a
link = Link newChildren []


compressChildren :: Children a -> Children a
compressChildren = fmap $ \a -> case a of
    Empty -> Empty
    Link chn _ -> let chn' = compressChildren chn in
        if all isEmpty chn' then Empty else a
    Node chn _ -> a
        { trieChildren = compressChildren chn }


keyBounds :: (Char, Char)
keyBounds = (toEnum 0x20, toEnum 0x7e)


mkChildren :: [(Char, Trie a)] -> Children a
mkChildren = array keyBounds


newChildren :: Children a
newChildren = mkChildren [(k, Empty) | k <- range keyBounds]


oneChild :: Char -> Trie a -> Children a
oneChild !k trie = newChildren // [(k, trie)]


getNonEmpties :: Trie a -> [Trie a]
getNonEmpties Empty = []
getNonEmpties trie = filter (not . isEmpty)
    (elems $! trieChildren trie)


zipChildren :: (Trie a -> Trie a -> Trie a)
            -> Children a -> Children a -> Children a
zipChildren f chn1 chn2 = mkChildren [(k, f c1 c2) |
    k <- indices chn1,
    c1 <- elems chn1,
    c2 <- elems chn2]


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


data KeyDiff
    = Equal
    | NoPrefix
    | LeftEx String String
    | RightEx String String
    | Diff String String String
    deriving (Show, Eq)

-- |Returns `(prefix, rest1, rest2)` where `prefix` is
-- the longest shared prefix, `rest1` and `rest2`
-- correspond to what is after the prefix in the first
-- and second arguments
keyDiff :: String -> String -> KeyDiff
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
    


-- |Creates a `Key` from a string
-- mkKey :: String -> Key
-- mkKey [] = undefined
-- mkKey (c:cs) = Key c chn


-- |Converts a `Key` back into a string
-- unKey :: Key -> String
-- unKey (Key ch rest) = (ch:rest)



-- instance Show a => Show (Trie a) where
--     show Empty = "empty"
--     show (Link chn) = "fromList" ++ show (assocs chn)
--     show (Node chn a) = "fromList [" ++ dropEnd 2 (
--         foldr (\(k,t) b -> let key = unKey k in
--             if isEmpty t then b else b ++
--                 "("++show key++", "++show t++"), "
--         )) ++ "]"

-- instance Ord a => Ord (Trie a) where
--     compare Empty Empty = EQ
--     compare Empty _ = LT
--     compare (Link _ _) Empty = GT
--     compare (Link chn1) (Link chn2) = compare chn1 chn2
--     compare (Link chn) n@(Node _ _) = foldr (\a' b -> 
--         let cmp = compare a' n in case (b, cmp) of
--             -- TODO: how to resolve dichotimous conflict?
--             (LT, GT) -> EQ -- ???
--             (GT, LT) -> EQ -- ???
--             _ -> cmp
--             ) EQ chn
--     compare (Node _ _) Empty = GT
--     compare n@(Node _ _) l@(Link _ _) = compare l n
--     compare (Node _ a1) (Node _ a2) = compare a1 a2

-- instance Functor Trie where
--     fmap _ Empty = Empty
--     fmap f (Link chn com) = let chn' = fmap f <$> chn in
--         Link chn' com
--     fmap f (Node chn a) = Node chn' a'
--         where
--             !chn' = fmap f <$> chn
--             !a' = f a

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


-- instance Semigroup (Trie a) where
--     (<>) = union


-- instance Monoid (Trie a) where
--     mempty = empty
