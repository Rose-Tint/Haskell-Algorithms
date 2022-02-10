module Algo.Insertion where


insertion :: (Ord a) => [a] -> [a]
insertion [] = []
insertion [x] = [x]
insertion l = helper [] l
    where
        helper done [] = done
        helper done (x:xs) = helper (insert done x) xs
        insert [] y = [y]
        insert (x:xs) y =
            -- use (>=) because we're putting
            -- the list in descending order
            -- first
            if y <= x then
                (y:x:xs)
            else
                (x:insert xs y)
