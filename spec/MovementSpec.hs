module MovementSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "pending" $ do
        it "is pending" $ do
            pending
