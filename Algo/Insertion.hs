module Algo.Insertion where


insSort :: (Ord a) => [a] -> [a]
insSort l = helper [] l
    where
        -- put in descending order first, then
        -- reverse because that is O(2n) instead
        -- of O(n^2)
        helper done [] = reverse done
        helper done (x:xs) = helper (insert done x) xs
        insert [] y = [y]
        insert (x:xs) y =
            -- use (>=) because we're putting
            -- the list in descending order
            -- first
            if y >= x then
                (y:x:xs)
            else
                (x:insert xs y)
