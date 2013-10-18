module BoardSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Applicative
import Board

main :: IO ()
main = hspec $ do
    describe "generateBoard" $ do
        describe "returns a board with property" $ do
            it "has 13 pots" $ property $
                \x -> length (generateBoard x) == 13

            it "7th pot is empty" $ property $
                \x -> potMarbles (generateBoard x !! 6) == 0

            it "the 7th pot is a store" $ property $
                \x -> isStore $ generateBoard x !! 6

            it "all pots except the 7th have marbles" $ property $
                \x -> all ((== x) . potMarbles) (allBut7thPot x)

  where
    allBut7thPot :: Int -> [Pot]
    allBut7thPot potCount = (++) <$> take 6 <*> drop 7 $ generateBoard potCount
