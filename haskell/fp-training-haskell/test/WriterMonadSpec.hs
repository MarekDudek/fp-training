module WriterMonadSpec where

import Data.Monoid
import Data.Foldable
import Control.Monad.Writer.Lazy
import Control.Monad.State

import MonadWriterTutorial

import Test.Hspec


main :: IO ()
main = hspec spec


spec :: Spec
spec = 

    describe "Writer monad" $ do

        it "Factorial for 0" $ do
            runWriter (fact1 0) `shouldBe` (1, "")




