module MaybeWrappersSpec where


import Data.Semigroup
import Data.Monoid
import Data.List.NonEmpty
import Control.Monad
import Safe

import Test.Hspec


main :: IO ()
main = hspec spec


spec :: Spec
spec = 

    describe "Maybe wrappers" $ do

        describe "Maybe of semigroup" $ do

            it "minimum of two numbers" $ do
                Just (Min 2) <> Just (Min 5) `shouldBe` Just (Min 2)

            it "minimum of one and empty" $ do
                Just (Min 2) <> Nothing `shouldBe` Just (Min 2)

            it "minimum of two empties" $ do
                Nothing <> Nothing `shouldBe` (Nothing :: Maybe (Min Int))

            it "minimum of many empties" $ do
                sconcat (Nothing :| [Nothing, Nothing]) `shouldBe` (Nothing :: Maybe (Min Int))

        describe "Maybe of monoid" $ do

            it "wrapper of monoid" $ do
                Just (Sum 2) <> Just (Sum 5) `shouldBe` Just (Sum 7)

            it "wrapper of monoid with empty value" $ do
                Nothing <> Just (Sum 5) `shouldBe` Just (Sum 5)

            it "concaenation of wrappers" $ do
                mconcat [Just (Sum 2), Just (Sum 3), Just (Sum 4)] `shouldBe` Just (Sum 9)


        describe "simple safe parsing" $ do

            it "reading integer from string" $ do
                (readMay "23" :: Maybe Int) `shouldBe` Just 23

            let read = readMay :: String -> Maybe Int

            it "reading integer from string simpler" $ do
                read "23" `shouldBe` Just 23

            it "reading integer can fail" $ do
                read "blah blah blah" `shouldBe` Nothing

            it "we can ignore parsing failures easily" $ do
                fmap Sum (read "1")  <>  fmap Sum (read "two")  <>  fmap Sum (read "3") `shouldBe` Just (Sum 4)
