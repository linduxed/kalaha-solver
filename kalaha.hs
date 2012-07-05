-- Imports {{{
import Data.List
import Data.Ord
-- }}}

-- Data types {{{
data Pot = Pot {
    marbleCount :: Int,
    isStore     :: Bool,
    position    :: Int
} deriving (Show)
--}}}

-- Helper functions {{{
isPotEmpty :: Pot -> Bool
isPotEmpty pot = marbleCount pot == 0

-- The Store of the opponent (the 14th pot) isn't included since marbles are
-- never dropped there.
generatePotList :: Int -> [Pot]
generatePotList startingMarbles = [Pot (if n == 7 then 0 else startingMarbles) (n == 7) n | n <- [1..13]]
-- }}}

-- Marble movement {{{
{-
 - Initiates the movement. Currently only used to prevent erroneous starting pots.
 -}
makeStartingMove :: [Pot] -> Int -> ([Pot], Bool)
makeStartingMove listOfPots startingPot
    | startingPot > 6                                       = error "Can't take from the store or opponents pots."
    | startingPot < 1                                       = error "Can't take from a pot before the first one."
    | isPotEmpty $ head $ drop (startingPot - 1) listOfPots = error "Can't start from an empty pot."
    | otherwise                                             = moveMarbles ((listOfPots, False), 0, True) startingPot

{-
 - Determines whether another lap is necessary.
 -}
moveMarbles :: (([Pot], Bool), Int, Bool) -> Int -> ([Pot], Bool)
moveMarbles ((listOfPots, landedInStore), marblesInHand, mustContinue) startingPot = resultingPotsAndStoreState where
    resultingPotsAndStoreState = lapLoop listOfPots landedInStore marblesInHand mustContinue startingPot

    lapLoop listOfPots landedInStoreLastLap marblesLeftFromLastLap mustContinue startingPot
        | not $ mustContinue = (listOfPots, landedInStoreLastLap)
        | otherwise          = moveMarbles (moveOneLap listOfPots startingPot marblesLeftFromLastLap) 0

{-
 - Does the actual movement of marbles.
 - The return type is huge and ugly because I don't know how to otherwise pass
 - the necessary information to moveMarbles, which needs to determine whether
 - another lap is necessary (and if so, how many marbles are still held in the
 - hand).
 - The top case in each of the loop sections only happens the first time the
 - loop is called (it's the only time no marbles are held).
 -}
moveOneLap :: [Pot] -> Int -> Int -> (([Pot], Bool), Int, Bool)
moveOneLap listOfPots startingPot startingMarblesInHand = ((modifiedPots, landedInStore), marblesLeftInHand, mustDoAnotherLap) where
    (newPots, marblesLeftInHand, mustDoAnotherLap, landedInStore) = moveLoop toTraverse startingMarblesInHand []
    (untouchedFirstPots, toTraverse)                              = splitAt (startingPot - 1) listOfPots
    modifiedPots                                                  = untouchedFirstPots ++ newPots

moveLoop [] marblesInHand outList = (reverse outList, marblesInHand, True, False)
moveLoop (x:xs) marblesInHand outList
    | marblesInHand == 0 = moveLoop xs (marbleCount x)     (returnEmptyPot x : outList)
    | marblesInHand >  1 = moveLoop xs (marblesInHand - 1) (returnPotWithOneMoreMarble x : outList)
    | marblesInHand /= 1 = error "marblesInHand was neither 0 nor >1, but somehow isn't 1"
    | isStore x          = (finishedPots, 0, False, True)
    | isPotEmpty x       = (finishedPots, 0, False, False)
    | otherwise          = moveLoop xs (marbleCount x + 1) (returnEmptyPot x : outList)
    where
        returnPotWithOneMoreMarble pot = pot { marbleCount = (marbleCount pot + 1) }
        returnEmptyPot pot             = pot { marbleCount = 0 }
        finishedPots                   = (reverse outList) ++ [returnPotWithOneMoreMarble x] ++ xs
-- }}}

-- Starting move branching {{{
pickAllPaths :: [Pot] -> [([Pot], [Int])]
pickAllPaths startingListOfPots = resultingPotsAndPaths where
    resultingPotsAndPaths = branchLoop startingListOfPots []

    branchLoop :: [Pot] -> [Int] -> [([Pot], [Int])]
    branchLoop listOfPots pathTaken
        | null validStartingPositions = [(listOfPots, pathTaken)]
        | otherwise                   = loopHelper validStartingPositions listOfPots pathTaken []
        where
            validStartingPositions = map position $ filter (not . isPotEmpty) potsOwnedByPlayer
            potsOwnedByPlayer      = take 6 listOfPots

    loopHelper :: [Int] -> [Pot] -> [Int] -> [([Pot], [Int])] -> [([Pot], [Int])]
    loopHelper [] _ _ returnList = returnList
    loopHelper (x:xs) listOfPots pathTaken returnList
        | not $ landsInStore = loopHelper xs listOfPots pathTaken combinedList 
        | otherwise          = branchLoop resultingPots (pathTaken ++ [x]) ++ loopHelper xs listOfPots pathTaken returnList
        where
            (resultingPots, landsInStore) = makeStartingMove listOfPots x
            combinedList                  = ((resultingPots, (pathTaken ++ [x])) : returnList)
-- }}}

-- {{{ Sorting
sortByMostInStore :: [([Pot], [Int])] -> [([Pot], [Int])]
sortByMostInStore inList = sortBy compareMostInStore inList where
    compareMostInStore a b
        | marblesInStore a > marblesInStore b = GT
        | marblesInStore a < marblesInStore b = LT
        | pathLength a     < pathLength b     = GT
        | pathLength a     > pathLength b     = LT
        | otherwise                           = EQ
        where
            marblesInStore x = marbleCount $ head $ drop 6 $ fst x
            pathLength x     = length $ snd x
-- }}}

-- Debug {{{
-- Prints a string with marble counts. Store is highlighted.
listMarbleCounts :: ([Pot], a) -> String
listMarbleCounts (listOfPots, _) = potStatus where
    potStatus = concat $ intersperse " - " (marbleAmounts listOfPots)

    marbleAmounts [] = []
    marbleAmounts (x:xs)
        | isStore x = ("_" ++ show (marbleCount x) ++ "_") : marbleAmounts xs
        | otherwise = show (marbleCount x) : marbleAmounts xs
-- }}}
