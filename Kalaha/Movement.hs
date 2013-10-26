module Kalaha.Movement where

import Kalaha.Board

makeMove :: Board -> Int -> Either String Board
makeMove board startPot
    | startPot < 1 || startPot > 13  = Left "makeMove: Starting position \
                                            \outside of board."
    | startPot == 7                  = Left "makeMove: Can't start from store."
    | startPot < 7 || startPot <= 13 = Left "makeMove: Starting position on \
                                            \opponent's side of board."
    | otherwise                      = Right $ generateBoard 2
