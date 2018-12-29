import Test.Hspec
import Lib
import Data

gwc = gridWithCoords grid

testFindWord word = 
    let Just result = findWord gwc word
        string = map cell2char result
    in string `shouldBe` word

testFindWords words = 
    let result = findWords gwc words
    in map (map cell2char) result

main :: IO ()
main = hspec $ do
    describe "fromatGrid" $ do
        it "Should concatenate every line with a new line" $ do
            (formatGrid (gridWithCoords ["abc", "def", "ghi"])) `shouldBe` "abc\ndef\nghi\n"
    describe "findWord" $ do
        it "Should find words that exist on the grid" $ do
            testFindWord "HASKELL"
            testFindWord "PERL"
        it "Shouldn't find words on the grid" $ do
            findWord gwc "XYZ" `shouldBe` Nothing
            findWord gwc "perl" `shouldBe` Nothing
    describe "findWords" $ do
        it "Should find all the words that exist on the grid" $ do
            testFindWords languages `shouldBe` languages
        it "Shouldn't find words on the grid" $ do
            findWords gwc ["XYZ", "ABC", "DFE"] `shouldBe` []
        it "Should find only the words that exist on the grid" $ do
            testFindWords ("ADA" : languages) `shouldBe` languages