module KalahSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Test.QuickCheck
import Kalaha.Board
import Kalaha.Kalaha

main :: IO ()
main = hspec $ do
    describe "makeStartingMove" $ do
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

                makeStartingMove startingBoard 5 `shouldBe` expectedBoard

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

                makeStartingMove startingBoard 1 `shouldBe` expectedBoard

        context "trying to start from anywhere but your pots" $ do
            it "raises an error" $ property $ \x -> (x < 1 || x > 13) ==> do
                let startingBoard = generateBoard 2

                makeStartingMove startingBoard x `shouldSatisfy` isLeft
