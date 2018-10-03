module SemigroupsSpec where


import Data.Semigroup
import Data.List.NonEmpty

import Test.Hspec


main :: IO ()
main = hspec spec


spec :: Spec
spec = 
    describe "Semigroups" $ do
        describe "minimum of numbers" $ do
            it "wrapper" $
                getMin (Min 5) `shouldBe` 5
            it "of two" $
                Min 2 <> Min 5 `shouldBe` Min 2 
            let l = Min 5 :| Min 1 : Min 4 : Min 2 : Min 3 : [] 
            it "of non-empty list" $ 
                sconcat l `shouldBe` Min 1 
        describe "maximum of numbers" $ do
            it "wrapper" $
                getMax (Max 5) `shouldBe` 5
            it "of two" $
                Max 2 <> Max 5 `shouldBe` Max 5 
            let l = Max 5 :| Max 1 : Max 4 : Max 2 : Max 3 : [] 
            it "of non-empty list" $ 
                sconcat l `shouldBe` Max 5
