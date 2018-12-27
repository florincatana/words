module Lib
    ( outputGrid
    , formatGrid
    , findWords
    , getGridLines
    , skewGrid
    ) where

import Data.List(isInfixOf, transpose)
import Data.Maybe(catMaybes)

type Grid = [String]

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid -> String
formatGrid = unlines

skewGrid :: Grid -> Grid
skewGrid [] = []
skewGrid (l:ls) = l : skewGrid (map indent ls)
        where indent line = '_' : line

getGridLines :: Grid -> [String]
getGridLines grid = 
    let horizontal = grid
        vertical = transpose grid
        diagonal1 = diagonalize grid
        diagonal2 = diagonalize (map reverse grid)
        lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
        in lines ++ (map reverse lines)
        
diagonalize :: Grid -> Grid
diagonalize = transpose . skewGrid

findWord :: Grid -> String -> Maybe String
findWord grid word =
    let lines = getGridLines grid
        found = or $ map (findWordInLine word) lines
    in if found then Just word else Nothing

findWords :: Grid -> [String] -> [String]
findWords grid words =
    let foundWords = map (findWord grid) words
    in catMaybes foundWords

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

