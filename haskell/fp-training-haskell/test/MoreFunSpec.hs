module MoreFunSpec where


import Data.Semigroup
import Data.Monoid

import Control.Monad
import Data.Functor.Identity

import Test.Hspec


main :: IO ()
main = hspec spec


spec :: Spec
spec = 

    describe "More fun" $ do

        describe "Dual" $ do

            it "original" $ do
                "Hello, " <> "World!" `shouldBe` "Hello, World!"

            it "wrapper" $ do
                Dual "Hello, " <> Dual "World!" `shouldBe` Dual "World!Hello, "

            let strings = ["quick", "brown", "fox"]

            it "wrappers" $ 
                let wrappers = fmap Dual strings in
                mconcat wrappers `shouldBe` Dual "foxbrownquick"

            it "originals with id function" $
                let plains = fmap id strings in
                mconcat plains `shouldBe` "quickbrownfox"

            it "originals with Identity monoid" $
                let plains = fmap Identity strings in
                mconcat plains `shouldBe` Identity "quickbrownfox"
            
