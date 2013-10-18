module Board where

data Pot = Pot {
    potPosition :: Int,
    potMarbles  :: Int,
    isStore  :: Bool
} deriving (Show)

type Board = [Pot]

generateBoard :: Int -> Board
generateBoard startingMarbles = map generatePot [1..13]
  where
    generatePot position
        | position /= 7 = Pot position startingMarbles False
        | otherwise     = Pot position 0 True
