module Lib
    ( mainGrid
    , languages
    , outputGrid
    , formatGrid
    ) where

type Grid = [String]

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid -> String
formatGrid = unlines

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