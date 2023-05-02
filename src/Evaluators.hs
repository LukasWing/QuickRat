{-# OPTIONS_GHC -Wno-unused-imports #-}
module Evaluators where
import Generators
import Helpers
import Test.QuickCheck
import Rattus.Stream hiding (const)
import Rattus
import qualified Data.Set as Set
import LTL
import Control.Monad
import Debug.Trace (trace)
import Functions
-- Foundations ----------------------------------------------------------------

-- State Machines -------------------------------------------------------------

tautologySM' :: Stamate a
tautologySM' = Pass

tautologySM :: Stamate a
tautologySM = NextT (const Pass)

contradictionSM :: Stamate a
contradictionSM = NextT (const Fail)

isConstSM :: Eq a => Maybe a -> Stamate a
isConstSM input =
  case input of
    Just value ->
      NextT
        ( \inp ->
            if inp == value
              then isConstSM (Just inp)
              else Fail
        )
    Nothing -> NextT (isConstSM . Just)

isHeadEqualSM ::(Eq a) => Str a -> Stamate a
isHeadEqualSM (h ::: _) =
    NextT (\value ->
            if h == value
                then Pass
                else Fail
            )


isPositive :: Stamate Int
isPositive = NextT (\n -> if n > 0 then isPositive else Fail)


-- Testables ------------------------------------------------------------------

isConstCheck :: (Eq a) => Str a -> Bool
isConstCheck aStr = stamateRun aStr (isConstSM Nothing)

isConstVal :: (Eq a) => a -> Str a -> Bool
isConstVal val aStr = stamateRun aStr (isConstSM (Just val))

areIncreasing :: (Ord a) => Str a -> Bool
areIncreasing (h1 ::: t1) =
    let isIncreasing e1 (e2 ::: t2) checksLeft =
            checksLeft == 0
            || (e1 <= e2)
            && isIncreasing e2 (adv t2) (checksLeft - 1)
        checks = (20::Int)
    in isIncreasing h1 (adv t1) checks

allTrue :: Str Bool -> Bool
allTrue aStr = stamateRun aStr $ isConstSM (Just True)

allFalse :: Str Bool -> Bool
allFalse aStr = stamateRun aStr $ isConstSM (Just False)

strProbEq :: Eq a => Str a -> Str a -> Bool
strProbEq s1 s2 = stamateRun s1 $ strProbEq' s2
    where
        strProbEq' (h ::: t) = NextT (
            \value ->
                if value == h
                    then strProbEq' (adv t)
                    else Pass
            )
