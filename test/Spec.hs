import Control.Monad
import Test.Tasty
--import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Data.Fix
import Data.Function
import Control.Comonad

import Day4Tests


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [day4Tests]

