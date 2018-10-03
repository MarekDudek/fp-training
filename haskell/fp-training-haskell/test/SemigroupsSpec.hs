module SemigroupsSpec where


import Data.Semigroup
import Data.List.NonEmpty
import Data.Set
import Control.Monad

import Test.Hspec


main :: IO ()
main = hspec spec

set = Data.Set.fromList


ns = 1 :| [2..5]
bs = False :| [True, False, True, False]
ss = set ['a'..'c'] :| [set ['b'..'d'], set ['c'..'e']]
ls = [1..3] :| [[4..6], [7..9]]
cs = "abc" :| ["def", "ghi"]


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
        describe "union of sets" $ do
            it "of two" $
                set ['a'..'c'] <> set ['b'..'d'] `shouldBe` set ['a'..'d']
            it "of non-empty list" $
                sconcat ss `shouldBe` set ['a'..'e']
        describe "appending lists" $ do
            it "of two" $
                [1..3] <> [4..6] `shouldBe` [1..6]
            it "of non-empty list" $
                sconcat ls `shouldBe` [1..9]
        describe "string concatenation" $ do
            it "of two" $
                "abc" <> "def" `shouldBe` "abcdef"
            it "of non-empty list" $
                sconcat cs `shouldBe` "abcdefghi"
