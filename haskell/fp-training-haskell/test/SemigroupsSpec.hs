module SemigroupsSpec where


import Data.Semigroup
import Data.List.NonEmpty
import Control.Monad

import Test.Hspec


main :: IO ()
main = hspec spec

ns = 1 :| [2, 3, 4, 5]
bs = False :| [True, False, True, False]

spec :: Spec
spec = 
    describe "Semigroups" $ do
        describe "minimum of numbers" $ do
            it "wrapper" $
                getMin (Min 5) `shouldBe` 5
            it "of two" $
                Min 2 <> Min 5 `shouldBe` Min 2 
            let mins = fmap Min ns
            it "of non-empty list" $ 
                sconcat mins `shouldBe` Min 1 
        describe "maximum of numbers" $ do
            it "wrapper" $
                getMax (Max 5) `shouldBe` 5
            it "of two" $
                Max 2 <> Max 5 `shouldBe` Max 5 
            let maxs = fmap Max ns
            it "of non-empty list" $ 
                sconcat maxs `shouldBe` Max 5
        describe "addition of numbers" $ do
            it "wrapper" $
                getSum (Sum 5) `shouldBe` 5
            it "of two" $
                Sum 2 <> Sum 5 `shouldBe` Sum 7
            let sums = fmap Sum ns
            it "of non-empty list" $ 
                sconcat sums `shouldBe` Sum 15
        describe "multiplication of numbers" $ do
            it "wrapper" $
                getProduct (Product 5) `shouldBe` 5
            it "of two" $
                Product 2 <> Product 5 `shouldBe` Product 10
            let products = fmap Product ns
            it "of non-empty list" $ 
                sconcat products `shouldBe` Product 120
        describe "multiplication of numbers" $ do
            it "wrapper" $
                getProduct (Product 5) `shouldBe` 5
            it "of two" $
                Product 2 <> Product 5 `shouldBe` Product 10
            let products = fmap Product ns
            it "of non-empty list" $ 
                sconcat products `shouldBe` Product 120
        describe "logical disjunction" $ do
            it "wrapper" $
                getAny (Any True) `shouldBe` True
            it "of two" $
                Any True <> Any False `shouldBe` Any True
            let anys = fmap Any bs
            it "of non-empty list" $ 
                sconcat anys `shouldBe` Any True
        describe "logical conjunction" $ do
            it "wrapper" $
                getAll (All True) `shouldBe` True
            it "of two" $
                All True <> All False `shouldBe` All False
            let alls = fmap All bs
            it "of non-empty list" $ 
                sconcat alls `shouldBe` All False
