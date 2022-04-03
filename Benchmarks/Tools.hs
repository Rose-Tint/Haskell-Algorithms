{-# LANGUAGE BangPatterns #-}

module Benchmarks.Tools where

import Control.DeepSeq (NFData, deepseq)
import Control.Monad ((<$!>))
import Data.Bits
import Data.Time.Clock.System
import Data.Word

default (Int)



type Millis = Word32


data Timer = Timer {
        tmMin :: Millis,
        tmMax :: Millis,
        tmIters :: !Int,
        tmTot :: !Word64
    }



nextRand :: Int -> Int
nextRand x = go (go (go x 15) (-17)) 5
    } where
        go !x' !n = x' `xor` (x' `shift` n)


benchmark :: Int -> Int -> (Int -> a -> b) -> b -> Benchmark
benchmark seed iters f b = 


{-
randRange :: Int -> Int -> IO Int
randRange mn mx = do
    n <- systemNanoseconds <$!> getSystemTime
    let !n' = fromIntegral n
    return $! n' - mn `mod` mx


randBool :: IO Bool
randBool = do
    n <- systemNanoseconds <$!> getSystemTime
    let !n' = fromIntegral n
    return $! toEnum $! n' .&. 1


-- i dont think this is right :(
sysTimeDiff :: SystemTime -> SystemTime -> SystemTime
sysTimeDiff (MkSystemTime s1 n1) (MkSystemTime s2 n2) =
    if n1 > n2 then
        MkSystemTime (s1 - s2) (n1 - n2)
    else
        MkSystemTime (s1 - s2 - 1000000000) (n2 - n1)


sysTimeAdd :: SystemTime -> SystemTime -> SystemTime
sysTimeAdd (MkSystemTime s1 n1) (MkSystemTime s2 n2) =
    MkSystemTime (s1 + s2) (n1 + n2)


benchmark :: NFData b => Int -> (a -> b) -> a -> IO Timer
benchmark itCount f a = go newTimer
    where
        go tm = if tmIters tm >= itCount then
                return tm
            else do
                st <- getSystemTime
                let x = f a
                end <- x `deepseq` getSystemTime
                let diff = sysTimeDiff st end
                putStrLn $! "Here "
                    ++ show (systemNanoseconds diff)
                go $! incTimer diff tm


sysTimeToDbl :: SystemTime -> Double
sysTimeToDbl (MkSystemTime s n) =
    realToFrac (s * 100) / (realToFrac n * 1e-6)


sysTimeToMs :: SystemTime -> Millis
sysTimeToMs (MkSystemTime s n) =
    fromIntegral (s * 100) +
    (n `quot` ((10 :: Word32) ^ (6 :: Word32)))


newTimer :: Timer
newTimer = Timer maxBound minBound 0 0


incTimer :: SystemTime -> Timer -> Timer
incTimer sys (Timer mn mx it tot)
    | ms < mn = tm { tmMin = ms }
    | ms > mx = tm { tmMax = ms }
    | otherwise = tm
    where
        tm = Timer mn mx (it + 1)
            (tot + fromIntegral ms)
        ms = sysTimeToMs sys


tmAvg :: Timer -> Millis
tmAvg (Timer _ _ it tot) = fromIntegral $!
    tot `div` fromIntegral it


shuffle :: String -> String
shuffle [] = []
shuffle [c] = [c]
shuffle str@(c:_) =
    if fromEnum c .&. (1 :: Int) == (0 :: Int) then
        shuffle left ++ shuffle right
    else
        shuffle right ++ shuffle left
    where
        (left, right) = splitAt (length str `div` 2) str
-}
