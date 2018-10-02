module SemigroupsSpec where


import Data.Semigroup
import Data.List.NonEmpty

import Test.Hspec


main :: IO ()
main = hspec spec


spec :: Spec
spec = 
    describe "Semigroups" $ 
        describe "minimum of numbers" $ do
            it "calculates minimum of two numbers" $
                Min 2 <> Min 5 `shouldBe` Min 2 
            it "calculates minimum on non-empty list of numbers" $ 
                let 
                    l = Min 5 :| Min 1 : Min 4 : Min 2 : Min 3 : [] 
                in 
                    sconcat l `shouldBe` Min 1 
