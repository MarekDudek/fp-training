module MonadWriterTutorial where


import Data.Monoid
import Data.Foldable
import Control.Monad.Writer.Lazy
import Control.Monad.State



fact1 :: Integer -> Writer String Integer
fact1 0 = return 1
