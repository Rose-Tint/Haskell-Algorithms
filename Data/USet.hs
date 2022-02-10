module Data.USet (
    module Data.Tree,
    USet,
    insert,
    search
) where

import Data.Tree hiding (insert, search)
import Algo.Hash


type USet a = Tree a


insert :: (Hashable a) => USet a -> a -> USet a
insert = finsert hashCmp


search :: (Hashable a) => USet a -> a -> USet a
search = finsert hashCmp
