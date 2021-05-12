import           Test.Syd

main :: IO ()
main = sydTest $ do
    describe "addition" $ do
        it "adds 3 to 5 to result in 8" $ 3 + 5 `shouldBe` 8
        it "adds 4 to 7 to result in 11" $ 4 + 7 `shouldBe` 11
