{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Generators where
import Helpers
import Rattus.Stream hiding (const)
import Rattus
import Rattus.Primitives
import Test.QuickCheck hiding ((.&.))
import Data.Bits ( Bits((.&.), (.|.)) )
import Control.Monad.State
import qualified Data.Set as Set
import System.Posix.Internals (statGetType)

-- Foundations -------------------------------------------------------------
instance (Arbitrary a) => Arbitrary (Str a) where
    arbitrary = do
        x <- arbitrary::Gen a
        xs <- arbitrary::Gen (Str a)
        return $ x:::delay xs

data Stamage a = Stamage {
    gen:: Gen a,
    next:: a -> Stamage a
}

stamageGen :: Stamage a -> Gen (Str a)
stamageGen aStamage = do
    v <- gen aStamage
    t <- stamageGen (next aStamage v)
    return $ v ::: delay t

data Stama a = Stama {
    genElem:: a,
    nextStr:: a -> Stama a
}

stamageStr :: Stama a -> Str a
stamageStr aStama = v ::: delay t
    where   v = genElem aStama
            t = stamageStr (nextStr aStama v)


-- State Machines ------------------------------------------------------------
increasingSM :: (Arbitrary a, Num a) => a -> Stamage a
increasingSM current = Stamage {
    gen = do
        addend <- arbitrary
        return (current + abs addend),
    next = \prev' -> increasingSM (prev' + current)
}

oddEven :: Stamage Int
oddEven = Stamage {gen = (.|. 1) <$> (arbitrary:: Gen Int), next = const evenOdd}

evenOdd :: Stamage Int
evenOdd = Stamage {gen = (*2) <$> (arbitrary:: Gen Int), next = const oddEven}

uniqueSM ::  (Arbitrary a, Ord a) => Set.Set a -> Stamage a
uniqueSM acc = Stamage {
    gen = arbitrary `suchThat`  (\n -> not (Set.member n acc)),
    next = \e -> uniqueSM $ Set.insert e acc
}

constSM :: (Arbitrary a) => Maybe a -> Stamage a
constSM input = Stamage {
    gen = maybe arbitrary return input,
    next = constSM . pure
}

constOf :: a -> Stamage a
constOf value = Stamage {
    gen = return value,
    next = const $ constOf value
}

cycleOf :: ([a], Int) -> Stamage a
cycleOf (aList, index) = Stamage {
    gen = return $ aList !! (index `mod` length aList),
    next = const $ cycleOf (aList, index + 1)
}

cycleOfStr :: ([a], Int) -> Stama a
cycleOfStr (aList, index) = Stama {
    genElem = aList !! (index `mod` length aList),
    nextStr = const $ cycleOfStr (aList, index + 1)
}
padStr :: ([a], Int) -> Stama a
padStr (aList, index) = Stama {
    genElem = if index < length aList
                then aList !! index
                else last aList,
    nextStr = const $ padStr (aList, index + 1)
}

makeRoundRobin :: [Gen a] -> Int -> Stamage a
makeRoundRobin gens index = Stamage {
    gen = gens !! index,
    next = \_ -> makeRoundRobin gens $ (index + 1) `mod` length gens
}

-- Stream Generators ----------------------------------------------------------
oddEvenGen :: Gen (Str Int)
oddEvenGen = stamageGen oddEven

evenOddGen :: Gen (Str Int)
evenOddGen = stamageGen evenOdd

increasingNums :: (Arbitrary a, Num a) => Gen (Str a)
increasingNums = stamageGen (increasingSM 0)

uniqueStr :: (Arbitrary a, Ord a) => Gen (Str a)
uniqueStr = stamageGen (uniqueSM Set.empty)

constStrSM ::(Arbitrary a) =>  Gen (Str a)
constStrSM = stamageGen (constSM Nothing)

constStrOf :: Arbitrary a => a -> Gen (Str a)
constStrOf value = stamageGen (constSM (Just value))

cyclicStrOf :: [a] -> Gen (Str a)
cyclicStrOf aList = return $ fixedCyclicStr aList

fixedCyclicStr :: [a] -> Str a
fixedCyclicStr aList = stamageStr $ cycleOfStr (aList, 0)

padFinite :: [a] -> Str a
padFinite aList = stamageStr $ padStr (aList, 0)

roundRobin :: [Gen a] -> Gen (Str a)
roundRobin gens = stamageGen $ makeRoundRobin gens 0