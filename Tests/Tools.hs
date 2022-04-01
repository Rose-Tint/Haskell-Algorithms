module Tests.Tools where

import Prelude hiding (exp)


data Asserter a
    = Success (a -> a -> Bool)
    | Failure (a -> a -> Bool)


data Test i o = Test {
        testName :: !String,
        testInput :: Maybe i,
        testOutput :: o
        testExpect :: o,
        testAsserter :: !Asserter
    }


test :: (Eq o) => String -> (i -> o) -> i -> o -> Test i o
test f inp exp = Test {
        testName = "",
        testInput = Just inp,
        testOutput = f inp,
        testExpect = exp,
        testAsserter = Success (==)
    }


assertTrue :: String -> (a -> a -> Bool) -> a -> a -> Test () o
assertTrue ass x y = Test "" Nothing x y (Success ass)
