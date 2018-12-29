module Main where

import Lib
import Data
import Data.Char
import System.IO
import System.Random
import System.Console.ANSI

main :: IO ()
main = do
    gen <- newStdGen
    let fillInGrid = fillInBlanks gen grid
        game = makeGame fillInGrid languages
    hSetBuffering stdout NoBuffering
    playTurn game

playTurn game = do
    colorDisplay . formatGame $ game
    if completed game 
        then do
            setSGR [SetColor Foreground Vivid Green]
            putStrLn "Congratulation! You have completed the Game!"
            setSGR []
            putStr ""
        else do
            setSGR [SetColor Foreground Vivid Blue]
            putStr "Please enter a word> "
            setSGR []
            putStr ""
            word <- getLine
            let newGame = playGame game word
            playTurn newGame

colorDisplay [] = putStr "\n\n"
colorDisplay out = do
    let char = head out
    if isUpper char then setSGR [SetColor Foreground Vivid Green] else setSGR []
    putStr $ char : []
    setSGR []
    putStr ""
    colorDisplay $ tail out
