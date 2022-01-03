module Tests.Test where


fassert :: String -> (a -> a -> Bool) -> a -> a -> IO ()
fassert n f a b = putStrLn $ "\t" ++ n ++ ": " ++ passStr
    where passStr = if f a b then "Pass" else "Fail"


assertEq :: (Eq a) => String -> a -> a -> IO ()
assertEq n = fassert n (==)
