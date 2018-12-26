module Lib
    ( someFunc,
    someString
    ) where

someFunc :: IO ()
someFunc = putStrLn someString

someString = "someString"
