import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "how to write a test" $ do
        it "Should be able to run tests" $ do
            someString `shouldBe` "someString"
