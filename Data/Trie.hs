module Data.Trie (Trie,
    -- Construction
    empty, singleton, fromList, fromFold,
    -- Insertion
    push, insert, insertWith,
    -- Query
    lookup, search, -- withPrefix, keysWithPrefix,
    (??),
    -- Deletion/Updating
    delete, adjust, update,
    -- Combination
    -- union, difference, intersect,
    -- Other
    isEmpty, toList, size, genSize, isMemberOf,
    compress,
    -- Pretty
    prettyTrie, printTrie,
) where

import Prelude hiding (lookup)

import Data.Trie.Internal
