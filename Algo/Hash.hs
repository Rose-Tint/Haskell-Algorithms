{-# LANGUAGE FlexibleInstances #-}

module Algo.Hash where

import Data.Ord
import Data.Char
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


instance Hashable Bool where
    hash True = 1
    hash False = 0


instance Hashable Char where
    hash = ord


instance Hashable Ordering where
    hash LT = -1
    hash EQ = 0
    hash GT = 1


instance (Hashable a) => Hashable [a] where
    hash = foldr combine' 0


instance Hashable () where
    hash _ = 0
