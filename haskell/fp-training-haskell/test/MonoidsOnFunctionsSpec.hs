module MonoidsOnFunctionsSpec where


import Data.Semigroup

import Test.Hspec


main :: IO ()
main = hspec spec


spec :: Spec
spec = 

    describe "Monoids on functions to monoids" $ do

        describe "Monoid for endomorphisms" $ do

            let e1 = Endo ("Hello, " ++)
            let e2 = Endo (++ "!")
            let computation = e1 <> e2

            it "appending endomorphisms" $ do
                appEndo computation "World" `shouldBe` "Hello, World!"

            it "folding endomorphisms" $ do
                appEndo (mconcat [e1, e2]) "World" `shouldBe` "Hello, World!"
