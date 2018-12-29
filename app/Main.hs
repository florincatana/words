module Main where

import Lib
import Data

main :: IO ()
main = 
    let game = makeGame grid languages
    in putStrLn . formatGame $ game