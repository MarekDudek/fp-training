module MoreMonoidsSpec where

import Test.Hspec

import Data.Semigroup
import Data.Monoid

main :: IO ()
main = hspec spec


newtype OnlyOne = OnlyOne { getOnlyOne :: Bool }

instance Semigroup OnlyOne where
    (OnlyOne p) <> (OnlyOne q) = OnlyOne ((p || q) && not(p && q))

instance Monoid OnlyOne where
    mempty = OnlyOne False

instance Eq OnlyOne where
    OnlyOne p == OnlyOne q = p == q

instance Show OnlyOne where
    show (OnlyOne p) = show p

spec :: Spec
spec = 

    describe "More monoids" $ do

        describe "Exclusive disjunction" $ do

            it "pending" $ do
                pending
