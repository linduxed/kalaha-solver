module Kalaha.Kalaha where

import Kalaha.Board
import Kalaha.Movement

makeStartingMove :: Board -> Int -> Either String Board
makeStartingMove board startPot
    | startPot < 1 || startPot > 13  = Left "makeStartingMove: Starting\
                                            \ position outside of board."
    | startPot == 7                  = Left "makeStartingMove: Can't start\
                                             \ from store."
    | startPot > 7 && startPot <= 13 = Left "makeStartingMove: Starting\
                                            \ position on opponent's side of\
                                            \ board."
    | otherwise                      = Right $ makeMove board startPot
