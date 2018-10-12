module MoreMonoidsSpec where

import Test.Hspec

import Data.Semigroup
import Data.Monoid

main :: IO ()
main = hspec spec


-- Only one

newtype OnlyOne = OnlyOne { getOnlyOne :: Bool } 
    deriving (Eq, Show)

instance Semigroup OnlyOne where
    OnlyOne p <> OnlyOne q = OnlyOne ((p || q) && not(p && q))

instance Monoid OnlyOne where
    mempty = OnlyOne False

-- All-or-None

newtype AllOrNone = AllOrNone { getAllOrNone :: Bool } 
    deriving (Eq, Show)

instance Semigroup AllOrNone where
    AllOrNone p <> AllOrNone q = AllOrNone (p == q)

instance Monoid AllOrNone where
    mempty = AllOrNone True



spec :: Spec
spec = 

    describe "More monoids" $ do

        describe "Exclusive disjunction" $ do

            it "pending" $ do
                pending
