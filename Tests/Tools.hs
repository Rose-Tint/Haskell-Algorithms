{-# LANGUAGE BangPatterns #-}

module Tests.Tools where

import Prelude hiding (exp)

import Control.Monad (foldM, when)



data Test i o = Test {
        testName :: !String,
        testFunc :: (i -> o),
        testAsserter :: (o -> o -> Bool),
        testPrintIn :: (Bool -> i -> IO ()),
        testPrintOut :: (Bool -> o -> IO ()),
        testPrintSucc :: !Bool
    }


data Results o
    = Pass o
    | Pass_
    | Fail


-- should this return `IO (Results ())` instead?
runTestUsing_ :: Test i o -> i -> o -> IO (Results ())
runTestUsing_ tst input exp = do
    let !res = (testFunc tst) input
        !testPassed = testAsserter tst exp res
        !res' = if testPassed then Pass_ else Fail
    putStrLn $! "Test " ++ show (testName tst) ++ ": " ++
        (if testPassed then "Pass" else "Fail")
    when (not testPassed || testPrintSucc tst && testPassed) $! do
        putStr "    Input    : "
        (testPrintIn tst) testPassed input
        putStr "    Output   : "
        (testPrintOut tst) testPassed res
        putStr "    Expected : "
        (testPrintOut tst) testPassed exp
    return res'


runTestUsing :: Test i o -> i -> o -> IO (Results o)
runTestUsing tst input exp = do
    let !res = (testFunc tst) input
        !testPassed = testAsserter tst exp res
        !res' = if testPassed then Pass res else Fail
    putStrLn $! "Test " ++ show (testName tst) ++ ": " ++
        (if testPassed then "Pass" else "Fail")
    when (not testPassed || testPrintSucc tst && testPassed) $! do
        putStr "    Input    : "
        (testPrintIn tst) testPassed input
        putStr "    Output   : "
        (testPrintOut tst) testPassed res
        putStr "    Expected : "
        (testPrintOut tst) testPassed exp
    return res'


foldTests :: Test (i, o) o -> o -> [(i, o)]
          -> IO (Results o)
foldTests _ !base [] = return $ Pass base
foldTests tst !base ((!input, !exp):is) = do
    mRes <- runTestUsing tst (input, base) exp
    case mRes `seq` mRes of
        Pass !res -> foldTests tst res is
        _ -> return Fail
    


runTestsUsing :: Test i o -> [(i, o)] -> IO (Results o)
runTestsUsing _ [] = return Pass_
runTestsUsing tst l = foldM (\ !lst (i, o) -> case lst of
    Fail -> return Fail
    _ -> runTestUsing tst i o
    ) Pass_ l
