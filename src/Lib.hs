module Lib
    ( mainGrid
    , languages
    , outputGrid
    , formatGrid
    , findWords
    , getGridLines
    ) where

import Data.List(isInfixOf, transpose)
import Data.Maybe(catMaybes)

type Grid = [String]

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid -> String
formatGrid = unlines

getGridLines :: Grid -> [String]
getGridLines grid = 
    let lines = grid ++ (transpose grid)
    in lines ++ (map reverse lines)

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

mainGrid = [ "__C________R___" 
            ,"__SI________U__"
            ,"__HASKELL____B_"
            ,"__A__A_____S__Y"
            ,"__R___B___C____"
            ,"__PHP____H_____"
            ,"____S_LREP_____"
            ,"____I__M_Y__L__"
            ,"____L_E__T_O___"
            ,"_________HB____"
            ,"_________O_____"
            ,"________CN_____"
            ]

languages = [ "BASIC"
            , "COBOL"
            , "CSHARP"
            , "HASKELL"
            , "LISP"
            , "PERL"
            , "PHP"
            , "PYTHON"
            , "RUBY"
            , "SCHEME"
            ]