module SemigroupsSpec where


import Data.Semigroup
import Data.List.NonEmpty
import Control.Monad

import Test.Hspec


main :: IO ()
main = hspec spec

nel = 1 :| [2, 3, 4, 5]

spec :: Spec
spec = 
    describe "Semigroups" $ do
        describe "minimum of numbers" $ do
            it "wrapper" $
                getMin (Min 5) `shouldBe` 5
            it "of two" $
                Min 2 <> Min 5 `shouldBe` Min 2 
            let mins = fmap Min nel
            it "of non-empty list" $ 
                sconcat mins `shouldBe` Min 1 
        describe "maximum of numbers" $ do
            it "wrapper" $
                getMax (Max 5) `shouldBe` 5
            it "of two" $
                Max 2 <> Max 5 `shouldBe` Max 5 
            let maxs = fmap Max nel
            it "of non-empty list" $ 
                sconcat maxs `shouldBe` Max 5
        describe "addition of numbers" $ do
            it "wrapper" $
                getSum (Sum 5) `shouldBe` 5
            it "of two" $
                Sum 2 <> Sum 5 `shouldBe` Sum 7
            let sums = fmap Sum nel
            it "of non-empty list" $ 
                sconcat sums `shouldBe` Sum 15
        describe "multiplication of numbers" $ do
            it "wrapper" $
                getProduct (Product 5) `shouldBe` 5
            it "of two" $
                Product 2 <> Product 5 `shouldBe` Product 10
            let products = fmap Product nel
            it "of non-empty list" $ 
                sconcat products `shouldBe` Product 120
