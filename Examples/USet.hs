module Main where

import Data.Bits

import Algo.Hash
import Structs.USet


data User
    = User {
        pin  :: Int,
        name :: String
    }
    deriving (Show, Eq)


instance Hashable User where
    hash = hash . pin


addUser :: Int -> USet User -> IO ()
addUser n us = do
    _ <- putStrLn "please enter your name:"
    nm <- getLine
    if nm == "stop" then
        print us
    else
        addUser n' (insert us (User n' nm))
        where
            n' = n `xor` (n * 2)


main :: IO ()
main = addUser 0x2a7be Empty
