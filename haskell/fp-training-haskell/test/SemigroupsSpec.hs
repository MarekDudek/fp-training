module SemigroupsSpec where


import Data.Semigroup
import Data.Monoid
import Data.List.NonEmpty hiding (fromList)
import Data.Set hiding (toList)
import Control.Monad

import Test.Hspec


main :: IO ()
main = hspec spec


spec :: Spec
spec = 

    describe "Semigroups and Monoids" $ do
        
        describe "on numbers" $ do

            let nel  = 1 :| [2..5] :: NonEmpty Int
            let list = toList nel  

            describe "minimum of numbers" $ do

                it "wrapper" $
                    getMin (Min 5) `shouldBe` 5

                it "of two" $
                    Min 2 <> Min 5 `shouldBe` Min 2 

                it "of non-empty list" $ 
                    let mins = fmap Min nel in
                    sconcat mins `shouldBe` Min 1 

                it "of list" $ 
                    let mins = fmap Min list in
                    mconcat mins `shouldBe` Min 1 

            describe "maximum of numbers" $ do

                it "wrapper" $
                    getMax (Max 5) `shouldBe` 5

                it "of two" $
                    Max 2 <> Max 5 `shouldBe` Max 5 

                it "of non-empty list" $ 
                    let maxs = fmap Max nel in
                    sconcat maxs `shouldBe` Max 5

                it "of list" $ 
                    let maxs = fmap Max list in
                    mconcat maxs `shouldBe` Max 5

            describe "addition of numbers" $ do

                it "wrapper" $
                    getSum (Sum 5) `shouldBe` 5

                it "of two" $
                    Sum 2 <> Sum 5 `shouldBe` Sum 7

                it "of non-empty list" $ 
                    let sums = fmap Sum nel in
                    sconcat sums `shouldBe` Sum 15

                it "of list" $ 
                    let sums = fmap Sum list in
                    mconcat sums `shouldBe` Sum 15

            describe "multiplication of numbers" $ do

                it "wrapper" $
                    getProduct (Product 5) `shouldBe` 5

                it "of two" $
                    Product 2 <> Product 5 `shouldBe` Product 10

                it "of non-empty list" $ 
                    let products = fmap Product nel in
                    sconcat products `shouldBe` Product 120

                it "of list" $ 
                    let products = fmap Product list in
                    mconcat products `shouldBe` Product 120

        describe "on booleans" $ do

            let nel  = False :| [True, False, True, False]
            let list = toList nel

            describe "logical disjunction" $ do

                it "wrapper" $
                    getAny (Any True) `shouldBe` True

                it "of two" $
                    Any True <> Any False `shouldBe` Any True

                it "of non-empty list" $ 
                    let anys = fmap Any nel in
                    sconcat anys `shouldBe` Any True

                it "of list" $ 
                    let anys = fmap Any list in
                    mconcat anys `shouldBe` Any True

            describe "logical conjunction" $ do

                it "wrapper" $
                    getAll (All True) `shouldBe` True

                it "of two" $
                    All True <> All False `shouldBe` All False

                it "of non-empty list" $ 
                    let alls = fmap All nel
                    sconcat alls `shouldBe` All False

                it "of list" $ 
                    let alls = fmap All list
                    mconcat alls `shouldBe` All False

        let set = fromList
        let nel = set ['a'..'c'] :| [set ['b'..'d'], set ['c'..'e']]
        let list = toList nel

        describe "union of sets" $ do

            it "of two" $
                set ['a'..'c'] <> set ['b'..'d'] `shouldBe` set ['a'..'d']
            it "of non-empty list" $
                sconcat nel  `shouldBe` set ['a'..'e']
            it "of list" $
                mconcat list `shouldBe` set ['a'..'e']

        let nel = [1..3] :| [[4..6], [7..9]]
        let list = toList nel

        describe "appending lists" $ do
            it "of two" $
                [1..3] <> [4..6] `shouldBe` [1..6]
            it "of non-empty list" $
                sconcat nel  `shouldBe` [1..9]
            it "of list" $
                mconcat list `shouldBe` [1..9]

        let nel  = "abc" :| ["def", "ghi"]
        let list = toList nel 

        describe "string concatenation" $ do
            it "of two" $
                "abc" <> "def" `shouldBe` "abcdef"
            it "of non-empty list" $
                sconcat nel  `shouldBe` "abcdefghi"
            it "of list" $
                mconcat list `shouldBe` "abcdefghi"
