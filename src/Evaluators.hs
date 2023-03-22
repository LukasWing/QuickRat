{-# OPTIONS_GHC -Wno-unused-imports #-}
module Evaluators where
import Generators hiding (next)
import Helpers
import Test.QuickCheck
import Rattus.Stream
import Rattus
import qualified Data.Set as Set

-- Foundations ----------------------------------------------------------------
data Stamate a = Stamate {
    check:: a -> Bool,
    next:: a -> Stamate a
}

stamateRun :: Str a -> Stamate a -> Bool
stamateRun aStr aStamate =
    let stamateRun' (h:::t) aStamate' checksLeft =
            checksLeft == 0
            || check aStamate h
            && stamateRun' (adv t) (next aStamate' h) (pred checksLeft)
        nChecks = 20
    in stamateRun' aStr aStamate nChecks

-- State Machines -------------------------------------------------------------

isUniqueSM :: (Ord a) => Set.Set a -> Stamate a
isUniqueSM aSet = Stamate {
    check = \val -> not (Set.member val aSet),
    next = \val -> isUniqueSM (Set.insert val aSet)
}

isConstSM :: Eq a => Maybe a -> Stamate a
isConstSM input = Stamate {
    check = \value -> case input of
                        Just val' -> val' == value
                        Nothing -> True,
    next = isConstSM . Just
}

-- Testables ------------------------------------------------------------------

areUnique :: (Ord a) => Str a -> Bool
areUnique aStr = stamateRun aStr $ isUniqueSM (Set.empty::Set.Set a)

isConstCheck :: (Eq a) => Str a -> Bool
isConstCheck aStr = stamateRun aStr (isConstSM Nothing)

areIncreasing :: (Ord a) => Str a -> Bool
areIncreasing (h1 ::: t1) =
    let isIncreasing e1 (e2 ::: t2) checksLeft =
            checksLeft == 0
            || (e1 <= e2)
            && isIncreasing e2 (adv t2) (checksLeft - 1)
        checks = (20::Int)
    in isIncreasing h1 (adv t1) checks

alternatesEvenOdd :: Str Int -> Int -> Bool
alternatesEvenOdd (h ::: t) checksLeft =
    checksLeft == 0
    || even h
    && alternatesOddEven (adv t) (checksLeft - 1)

alternatesOddEven :: Str Int -> Int -> Bool
alternatesOddEven (h ::: t) checksLeft =
    checksLeft == 0
    || odd h
    && alternatesEvenOdd (adv t) checksLeft

