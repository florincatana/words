module Lib
    ( grid
    , languages
    , outputGrid
    , formatGrid
    ) where

outputGrid :: [String] -> IO ()
outputGrid = putStrLn (formatGrid grid)

formatGrid :: [String] -> String
formatGrid = unlines

grid = [ "__c________R___" 
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
        ,"_________N_____"
        ]

languages = ["CHASRP"
            , "RUBY"
            , "HASKELL"
            , "COBOL"
            , "PYTHON"
            , "PERL"
            , "PHP"
            ]