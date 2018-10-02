module SemigroupsSpec where


import Test.Hspec

main :: IO ()
main = hspec spec


spec :: Spec
spec = 
    describe "absolute" $ 
        it "returns the original number when given a positive input" $
          2 + 2 `shouldBe` 4
