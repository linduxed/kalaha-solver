module Board where

data Pot = Pot {
    potPosition :: Int,
    potMarbles  :: Int,
    isStore  :: Bool
} deriving (Show)

type Board = [Pot]
