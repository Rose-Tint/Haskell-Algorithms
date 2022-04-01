{-# LANGUAGE BangPatterns #-}

module Tests.Tools where

import Prelude hiding (exp)

import Control.Monad (foldM)

import Text.Printf (printf)



data Test i o = Test {
        testName :: !String,
        testFunc :: (i -> o),
        testExpect :: o,
        testAsserter :: (o -> o -> Bool),
        testPrintIn :: (Bool -> i -> IO ()),
        testPrintOut :: (Bool -> o -> IO ())
    }


data Results o
    = Pass o
    | Pass_
    | Fail


-- should this return `IO (Results ())` instead?
runTestUsing_ :: Test i o -> i -> IO (Results o)
runTestUsing_ tst input = do
    let !res = (testFunc tst) input
        !exp = testExpect tst
        !testPassed = testAsserter tst exp res
    printf $! "Test " ++ testName tst
    if testPassed then do
        putStrLn " : Pass"
        return $ Pass_
    else do
        putStr " : Fail\n    Input    : "
        (testPrintIn tst) testPassed input
        putStr "    Output   : "
        (testPrintOut tst) testPassed res
        putStr "    Expected : "
        (testPrintOut tst) testPassed exp
        return Fail


runTestUsing :: Test i o -> i -> IO (Results o)
runTestUsing tst input = do
    putStr $! "Test " ++ show (testName tst)
    let !res = (testFunc tst) input
        !exp = testExpect tst
        !testPassed = testAsserter tst exp res
    if testPassed then do
        putStrLn " : Pass"
        return $ Pass res
    else do
        putStr " : Fail\n    Input    : "
        (testPrintIn tst) testPassed input
        putStr "    Output   : "
        (testPrintOut tst) testPassed res
        putStr "    Expected : "
        (testPrintOut tst) testPassed exp
        return Fail


foldTests :: Test (i, o) o -> o -> [(i, o)]
          -> IO (Results o)
foldTests _ !base [] = return $ Pass base
foldTests tst !base ((!input, !exp):is) = do
    let tst' = tst { testExpect = exp }
    mRes <- runTestUsing tst' (input, base)
    case mRes `seq` mRes of
        Pass !res -> foldTests tst res is
        _ -> return Fail
    


runTestsUsing :: Test i o -> [(i, o)] -> IO (Results o)
runTestsUsing _ [] = return Pass_
runTestsUsing tst l = foldM (\ !lst (i, o) -> case lst of
    Fail -> return Fail
    _ -> do
        let tst' = tst { testExpect = o }
        runTestUsing tst' i
    ) Pass_ l
