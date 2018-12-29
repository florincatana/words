module Lib
    ( outputGrid
    , formatGrid
    , findWord
    , findWords
    , getGridLines
    , skewGrid
    , zipOverGrid
    , zipOverGridWith
    , gridWithCoords
    , findWordInLine
    , findWordInCellLinePrefix
    , cell2char
    , Game (gameGrid, gameWords)
    , makeGame
    , totalWords
    , score
    , playGame
    , formatGame
    , completed
    , makeRandomGrid
    , fillInBlanks
    , formatGameGrid
    ) where

import Data.List(isInfixOf, transpose)
import Data.Maybe(catMaybes, listToMaybe)
import Data.Char (toLower)
import qualified Data.Map as M
import System.Random

data Cell = Cell (Integer, Integer) Char 
            | Indent
            deriving (Eq, Ord, Show)
type Grid a = [[a]]

data Game = Game {
    gameGrid :: Grid Cell,
    gameWords :: M.Map String (Maybe [Cell])
}   deriving Show

makeGame :: Grid Char -> [String] -> Game
makeGame grid words = 
    let gwc = gridWithCoords grid
        tuplify word = (word, Nothing)
        list = map tuplify words
        dict = M.fromList list
    in Game gwc dict

totalWords :: Game -> Int
totalWords grid = length . M.keys $ gameWords grid

score :: Game -> Int
score grid = length . catMaybes . M.elems $ gameWords grid

completed :: Game -> Bool
completed game = score game == totalWords game

playGame :: Game -> String -> Game
playGame game word | not $ M.member word (gameWords game) = game
playGame game word = 
    let grid = gameGrid game
        foundWord = findWord grid word
    in case foundWord of
        Nothing -> game
        Just cs -> 
            let dict = gameWords game
                newDict = M.insert word foundWord dict
            in game { gameWords = newDict }

formatGame :: Game -> String
formatGame game = formatGameGrid game
                ++ "\n\nscore: "
                ++ (show $ score game)
                ++ "/"
                ++ (show $ totalWords game)

formatGameGrid :: Game -> String
formatGameGrid game =
    let grid = gameGrid game
        dict = gameWords game :: M.Map String (Maybe [Cell])
        cellSet = concat . catMaybes . M.elems $ dict
        formatCell cell = 
                let char = cell2char cell
                in if cell `elem` cellSet then char else toLower char
        charGrid = mapOverGrid formatCell grid
    in unlines charGrid

makeRandomGrid gen = 
    let (gen1, gen2) = split gen
        row = randomRs ('A', 'Z') gen1
    in row : makeRandomGrid gen2

fillInBlanks gen grid =
    let randomGrid = makeRandomGrid gen
        fill '_' r = r
        fill c _ = c
    in zipOverGridWith fill grid randomGrid

zipOverGrid :: Grid a -> Grid b -> Grid (a,b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c  
zipOverGridWith = zipWith . zipWith

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

coordsGrid :: Grid (Integer, Integer)
coordsGrid = 
    let rows = map repeat [0..]
        cols = repeat [0..]
    in zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid

outputGrid :: Grid Cell-> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cell2char

cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Indent = '?'

skewGrid :: Grid Cell -> Grid Cell
skewGrid [] = []
skewGrid (l:ls) = l : skewGrid (map indent ls)
        where indent line = Indent : line

getGridLines :: Grid Cell -> [[Cell]]
getGridLines grid = 
    let horizontal = grid
        vertical = transpose grid
        diagonal1 = diagonalize grid
        diagonal2 = diagonalize (map reverse grid)
        lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
        in lines ++ (map reverse lines)
        
diagonalize :: Grid Cell -> Grid Cell
diagonalize = transpose . skewGrid

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
    let lines = getGridLines grid
        foundWords = map (findWordInLine word) lines
    in listToMaybe $ catMaybes foundWords

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words =
    let foundWords = map (findWord grid) words
    in catMaybes foundWords

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
    let found = findWordInCellLinePrefix [] word line
    in case found of
        Nothing -> findWordInLine word $ tail line
        cs@(Just _) -> cs

findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe[Cell]
findWordInCellLinePrefix acc (x:xs) (c:cs) | x == cell2char c
    = findWordInCellLinePrefix (c : acc) xs cs
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing
