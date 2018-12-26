module Lib
    ( mainGrid
    , languages
    , outputGrid
    , formatGrid
    , findWords
    ) where

import Data.List(isInfixOf)
import Data.Maybe(catMaybes)

type Grid = [String]

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid -> String
formatGrid = unlines

findWord :: Grid -> String -> Maybe String
findWord grid word =
    let lines = grid ++ (map reverse grid)
        found = or $ map (findWordInLine word) lines
    in if found then Just word else Nothing

findWords :: Grid -> [String] -> [String]
findWords grid words =
    let foundWords = map (findWord grid) words
    in catMaybes foundWords

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

mainGrid = [ "__c________R___" 
        ,"__SI________U__"
        ,"__HASKELL____B_"
        ,"__A___________Y"
        ,"__R____________"
        ,"__PHP__________"
        ,"____S_LREP_____"
        ,"____I____Y_____"
        ,"____L____T_____"
        ,"_________H_____"
        ,"_________O_____"
        ,"_________N_____"]

languages = ["CHASRP"
            , "RUBY"
            , "HASKELL"
            , "COBOL"
            , "PYTHON"
            , "PERL"
            , "PHP"]