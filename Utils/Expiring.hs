module Utils.Expiring where


data Expiring a =
    None | NTimes a Int | Never a
    deriving (Show, Ord, Eq)


instance Foldable Expiring where
    foldr f b None = b
    foldr f b (NTimes a n)
        | (n <= 0) = f a b
        | otherwise = foldr f (f a b) (NTimes a (n - 1))


expiring :: (a -> b) -> Expiring a -> Expiring b
expiring f (NTimes a n) = NTimes (f a) (n - 1)
expiring _ _ = None
