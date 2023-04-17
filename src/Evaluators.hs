{-# OPTIONS_GHC -Wno-unused-imports #-}
module Evaluators where
import qualified Generators as G
import Helpers
import Test.QuickCheck
import Rattus.Stream hiding (const)
import Rattus
import qualified Data.Set as Set
import Control.Monad (join)

-- Foundations ----------------------------------------------------------------
data Stamate a = Stamate {
    check:: a -> Bool,
    next:: a -> Stamate a
}

stamateRun :: Str a -> Stamate a -> Bool
stamateRun aStr aStamate =
    let stamateRun' (h:::t) aStamate' checksLeft =
            checksLeft == 0
            || check aStamate' h
            && stamateRun' (adv t) (next aStamate' h) (pred checksLeft)
        nChecks = 20
    in stamateRun' aStr aStamate nChecks

stamateRunStamage :: G.Stamage a -> Stamate a -> Bool
stamateRunStamage  aStamage aStamate =
    let stamateRunStamage' aStamage' aStamate' checksLeft =
            checksLeft == 0
            || check aStamate' h
            && stamateRunStamage' t (next aStamate' h) (pred checksLeft)
            where h = G.gen aStamage'
                  t = G.next aStamage' h

        nChecks = 20
    in stamateRunStamage' aStamage aStamate nChecks

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


isAlternatingSM :: (Integral a) => Bool -> Stamate a
isAlternatingSM expectEven = Stamate{
    check = \anInt  -> if expectEven 
                        then even anInt 
                        else odd anInt,
    next = \_ -> isAlternatingSM (not expectEven)
}

isAlternatingAB ::Eq a => (a, a) -> Stamate a
isAlternatingAB (current, next) = Stamate {
    check = (current ==),
    next = \_ -> isAlternatingAB (next, current)
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

alternatesEvenOdd :: Integral a => Str a -> Bool
alternatesEvenOdd aStr = stamateRun aStr (isAlternatingSM True)

alternatesOddEven :: Integral a => Str a -> Bool
alternatesOddEven aStr = stamateRun aStr (isAlternatingSM False)

allTrue :: Str Bool -> Bool
allTrue aStr = stamateRun aStr $ isConstSM (Just True)

allFalse :: Str Bool -> Bool
allFalse aStr = stamateRun aStr $ isConstSM (Just False)

alternatesAB :: Eq a => (a, a) -> Str a -> Bool
alternatesAB ab aStr = stamateRun aStr $ isAlternatingAB ab 

strProbEq :: Eq a => Str a -> Str a -> Bool
strProbEq s1 s2 = stamateRun s1 $ strProbEq' s2
    where 
        strProbEq' (h ::: t) = Stamate {
            check = (== h),
            next = const $ strProbEq' (adv t) 
        } 

