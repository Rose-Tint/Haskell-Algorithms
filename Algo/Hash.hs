{-# LANGUAGE FlexibleInstances #-}

module Algo.Hash where

import Data.Ord
import Data.Bits


type Hash = Int


class Hashable a where
    hash :: a -> Hash


hashCmp :: (Hashable a) => a -> a -> Ordering
hashCmp = comparing hash


combine :: Hash -> Hash -> Hash
combine a b = a `xor` (b + c + als + ars)
    where
        als = shift a (-6)
        ars = shift a 2
        c = 0x9e3779b9


combine' :: (Hashable a, Hashable b) => a -> b -> Hash
combine' a b = combine (hash a) (hash b)


instance Hashable Int where
    hash = id


instance Hashable Integer where
    hash = fromIntegral


instance (Hashable a, Foldable t) => Hashable (t a) where
    hash = foldr combine' 0

instance Hashable () where
    hash _ = 0
