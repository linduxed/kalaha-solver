module MovementSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Test.QuickCheck
import Kalaha.Movement
import Kalaha.Board

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "makeMove" $ do
        it "returns a board" $ do
