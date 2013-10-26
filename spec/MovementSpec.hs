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
