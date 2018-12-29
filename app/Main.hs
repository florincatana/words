module Main where

import Lib
import Data
import System.IO

main :: IO ()
main = do
    let game = makeGame grid languages
    hSetBuffering stdout NoBuffering
    playTurn game

playTurn game = do
    putStrLn . formatGame $ game
    if completed game then
        putStrLn "Congratulation! You have completed the Game!"
    else
        putStr "Please enter a word> "
    word <- getLine
    let newGame = playGame game word
    playTurn newGame