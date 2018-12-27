import Test.Hspec
import Lib
import Data

main :: IO ()
main = hspec $ do
    describe "fromatGrid" $ do
        it "Should concatenate every line with a new line" $ do
            (formatGrid ["abc", "def", "ghi"]) `shouldBe` "abc\ndef\nghi\n"
    describe "findWord" $ do
        it "Should find words that exist on the grid" $ do
            findWord grid "HASKELL" `shouldBe` Just "HASKELL"
            findWord grid "PERL" `shouldBe` Just "PERL"
        it "Shouldn't find words on the grid" $ do
            findWord grid "XYZ" `shouldBe` Nothing
            findWord grid "perl" `shouldBe` Nothing
    describe "findWords" $ do
        it "Should find all the words that exist on the grid" $ do
            findWords grid languages `shouldBe` languages
        it "Should find only the words that exist on the grid" $ do
            findWords grid ("ADA" : languages) `shouldBe` languages