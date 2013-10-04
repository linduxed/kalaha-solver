import Data.List

data Pot = Pot {
    marbleCount :: Int,
    isStore     :: Bool,
    position    :: Int
} deriving (Show)

data LapResult = LapContinue Int
               | LapLandedInStore
               | LapDone

isPotEmpty :: Pot -> Bool
isPotEmpty pot = marbleCount pot == 0

-- The Store of the opponent (the 14th pot) isn't included since marbles are
-- never dropped there.
generatePotList :: Int -> [Pot]
generatePotList startingMarbles = [Pot (if n == 7 then 0 else startingMarbles) (n == 7) n | n <- [1..13]]

{-
 - Initiates the movement. Currently only used to prevent erroneous starting pots.
 -}
makeStartingMove :: [Pot] -> Int -> ([Pot], Bool)
makeStartingMove listOfPots startingPot
    | startingPot > 6                              = error "Can't take from the store or opponents pots."
    | startingPot < 1                              = error "Can't take from a pot before the first one."
    | isPotEmpty $ listOfPots !! (startingPot - 1) = error "Can't start from an empty pot."
    | otherwise                                    = moveMarbles listOfPots startingPot 0

{-
 - Determines whether another lap is necessary.
 -}
moveMarbles :: [Pot] -> Int -> Int -> ([Pot], Bool)
moveMarbles listOfPots startingPot marblesInHand =
    let (newPots, resultOfLap) = moveOneLap listOfPots startingPot marblesInHand in
    case resultOfLap of
        LapContinue newMarblesInHand -> moveMarbles newPots 0 newMarblesInHand
        LapLandedInStore             -> (newPots, True)
        LapDone                      -> (newPots, False)

{-
 - Does the actual movement of marbles.
 -}
moveOneLap :: [Pot] -> Int -> Int -> ([Pot], LapResult)
moveOneLap listOfPots startingPot startingMarblesInHand = (modifiedPots, resultOfLap) where
    (newPots, resultOfLap)           = moveLoop toTraverse startingMarblesInHand []
    (untouchedFirstPots, toTraverse) = splitAt (startingPot - 1) listOfPots
    modifiedPots                     = untouchedFirstPots ++ newPots

{-
 - The top non-base-case only happens the first time the loop is called (it's
 - the only time no marbles are held).
 -}
moveLoop :: [Pot] -> Int -> [Pot] -> ([Pot], LapResult)
moveLoop [] marblesInHand outList = (reverse outList, LapContinue marblesInHand)
moveLoop (x:xs) marblesInHand outList
    | marblesInHand == 0 = moveLoop xs (marbleCount x)     (returnEmptyPot x : outList)
    | marblesInHand >  1 = moveLoop xs (marblesInHand - 1) (returnPotWithOneMoreMarble x : outList)
    | marblesInHand /= 1 = error "marblesInHand was neither 0 nor >1, but somehow isn't 1"
    | isStore x          = (finishedPots, LapLandedInStore)
    | isPotEmpty x       = (finishedPots, LapDone)
    | otherwise          = moveLoop xs (marbleCount x + 1) (returnEmptyPot x : outList)
    where
        returnPotWithOneMoreMarble pot = pot { marbleCount = marbleCount pot + 1 }
        returnEmptyPot pot             = pot { marbleCount = 0 }
        finishedPots                   = reverse outList ++ [returnPotWithOneMoreMarble x] ++ xs

pickAllPaths :: [Pot] -> [([Pot], [Int])]
pickAllPaths startingListOfPots = branchLoop startingListOfPots [] where
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
        | not landsInStore = loopHelper xs listOfPots pathTaken combinedList
        | otherwise        = branchLoop resultingPots (pathTaken ++ [x]) ++ loopHelper xs listOfPots pathTaken returnList
        where
            (resultingPots, landsInStore) = makeStartingMove listOfPots x
            combinedList                  = (resultingPots, pathTaken ++ [x]) : returnList

sortByMostInStore :: [([Pot], [Int])] -> [([Pot], [Int])]
sortByMostInStore = sortBy compareMostInStore where
    compareMostInStore a b
        | marblesInStore a > marblesInStore b = GT
        | marblesInStore a < marblesInStore b = LT
        | pathLength a     < pathLength b     = GT
        | pathLength a     > pathLength b     = LT
        | otherwise                           = EQ
        where
            marblesInStore = marbleCount . head . drop 6 . fst
            pathLength     = length . snd

-- Prints a string with marble counts. Store is highlighted.
listMarbleCounts :: ([Pot], a) -> String
listMarbleCounts (listOfPots, _) = potStatus where
    potStatus = intercalate " - " (marbleAmounts listOfPots)

    marbleAmounts [] = []
    marbleAmounts (x:xs)
        | isStore x = ("_" ++ show (marbleCount x) ++ "_") : marbleAmounts xs
        | otherwise = show (marbleCount x) : marbleAmounts xs
