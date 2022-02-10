module Main where

import Algo.Hash
import Structs.USet
import qualified Structs.MinHeap as H


data User
    = User {
        name :: String,
        highscore :: Int
    }
    deriving (Show, Eq)


instance Hashable User where
    hash = hash . name


addUser :: USet User -> IO ()
addUser users = do
    _ <- putStrLn "please enter your name:"
    nm <- getLine
    case nm of
        "show" -> do
            _ <- putStrLn $ prettyTree users
            addUser users
        "stop" ->
            putStrLn $ prettyTree users
        _      ->
            addUser (insert users (User nm))


main :: IO ()
main = addUser Empty
