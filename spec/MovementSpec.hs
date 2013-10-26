module MovementSpec where

import Test.Hspec
import Kalaha.Movement
import Kalaha.Board

main :: IO ()
main = hspec $ do
    describe "makeMove" $ do
        context "picking a marble which lands you in the store" $ do
            it "returns an altered board" $ do
                let startingBoard = generateBoard 2
                let expectedBoard = Right [ Pot 1 2 False
                                          , Pot 2 2 False
                                          , Pot 3 2 False
                                          , Pot 4 2 False
                                          , Pot 5 0 False
                                          , Pot 6 3 False
                                          , Pot 7 1 True
                                          , Pot 8 2 False
                                          , Pot 9 2 False
                                          , Pot 10 2 False
                                          , Pot 11 2 False
                                          , Pot 12 2 False
                                          , Pot 13 2 False ]

                makeMove startingBoard 5 `shouldBe` expectedBoard

        context "picking a marble which doesn't land in the store" $ do
            it "returns an altered board" $ do
                let startingBoard = generateBoard 2
                let expectedBoard = Right [ Pot 1 1 False
                                          , Pot 2 0 False
                                          , Pot 3 1 False
                                          , Pot 4 4 False
                                          , Pot 5 4 False
                                          , Pot 6 1 False
                                          , Pot 7 1 True
                                          , Pot 8 3 False
                                          , Pot 9 0 False
                                          , Pot 10 3 False
                                          , Pot 11 3 False
                                          , Pot 12 0 False
                                          , Pot 13 3 False ]

                makeMove startingBoard 1 `shouldBe` expectedBoard

        context "picking a marble from outside of the board" $ do
            it "raises an error" $ do
                let startingBoard = generateBoard 2
                let expectedError = Left "makeMove: Starting position outside \
                                         \of board."

                makeMove startingBoard (-1) `shouldBe` expectedError
                makeMove startingBoard 14 `shouldBe` expectedError

        context "picking a marble from other player's pots" $ do
            it "raises an error" $ do
                let startingBoard = generateBoard 2
                let expectedError = Left "makeMove: Starting position on \
                                         \opponent's side of board."

                makeMove startingBoard 8 `shouldBe` expectedError
                makeMove startingBoard 13 `shouldBe` expectedError

        context "trying to start from the store" $ do
            it "raises an error" $ do
                let startingBoard = generateBoard 2
                let expectedError = Left "makeMove: Can't start from store."

                makeMove startingBoard 7 `shouldBe` expectedError
