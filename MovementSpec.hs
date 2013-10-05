module MovementSpec where

import Test.Hspec
import Movement

main :: IO ()
main = hspec $ do
    describe "makeOneMove" $ do
        context "picking a marble which lands you in the store" $ do
            it "returns an altered board" $ do
                startingBoard <- Board 2
                expectedBoard <-
                    [ Pot 2, Pot 2, Pot 2, Pot 2, Pot 0, Pot 3
                    , Pot 1
                    , Pot 2, Pot 2, Pot 2, Pot 2, Pot 2, Pot 2 ]

                makeMove startingBoard 1 `shouldBe` expectedBoard
