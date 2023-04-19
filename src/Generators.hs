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
import LTL
import Data.Map (valid)

-- Foundations -------------------------------------------------------------
instance (Arbitrary a) => Arbitrary (Str a) where
    arbitrary = do
        x <- arbitrary::Gen a
        xs <- arbitrary::Gen (Str a)
        return $ x:::delay xs

type Stamage a = StamageP a
newtype StamageP a = Next (Gen (a, StamageP a))
stamagePGen :: StamageP a -> Gen (Str a)
stamagePGen (Next aGen) = do
    (value, aStamageP) <- aGen
    rest <- stamagePGen aStamageP
    return $ value ::: delay rest

-- State Machines ------------------------------------------------------------
arbitraryStamageP :: (Arbitrary a, Num a) => Stamage a
arbitraryStamageP = error "Not Implemented"

cycleOfP :: ([a], Int) -> Stamage a
cycleOfP = error "Not Implemented"

padStrP :: ([a], Int) -> Stamage a
padStrP = error "Not Implemented"


-- Stream Generators ----------------------------------------------------------
oddEvenGenP :: Gen (Str Int)
oddEvenGenP = stamagePGen oddEvenP

evenOddGenP :: Gen (Str Int)
evenOddGenP = stamagePGen evenOddP

increasingNums :: (Arbitrary a, Num a) => Gen (Str a)
increasingNums = error "Not Implemented" -- stamageGen (increasingSM 0)

uniqueStr :: (Arbitrary a, Ord a) => Gen (Str a)
uniqueStr = error "Not Implemented" -- stamageGen (uniqueSM Set.empty)

constStrP ::(Arbitrary a) =>  Gen (Str a)
constStrP = error "Not Implemented" -- stamageGen (constSM Nothing)

constStrOfP :: a -> Gen (Str a)
constStrOfP value = stamagePGen $ constOfP value

padFiniteP :: [a] -> Str a
padFiniteP aList = error "Not Implemented" -- stamageStr $ padStr (aList, 0)


--- New StamagePs ------------------------------------------------------

evenOddP :: StamageP Int
evenOddP = Next $ do
        value <- (*2) <$> (arbitrary:: Gen Int)
        return (value, oddEvenP)

oddEvenP :: StamageP Int
oddEvenP = Next $ do
        value <- (.|. 1) <$> arbitrary
        return (value, evenOddP)

constOfP :: a -> StamageP a
constOfP value = Next $ return (value, constOfP value)

--- StamageP combinators ---------------------------------------------
nextP :: Gen a -> StamageP a -> StamageP a
nextP nextGen aStamageP =
    Next $ do
        tip <- nextGen
        return (tip, aStamageP)

untilP :: Gen a -> StamageP a -> StamageP a
untilP tipGen aStamageP = 
    Next $ do
        nPrepends <- abs <$> (arbitrary :: Gen Int)
        let Next aGenStamage = applyN nPrepends (nextP tipGen) aStamageP
        aGenStamage

roundRobinP :: [Gen a] -> StamageP a
roundRobinP gens =
    let roundRobinP' gens' index = Next $ do
            value <- gens !! index
            let nextIndex = (index + 1) `mod` length gens'
            return (value, roundRobinP' gens' nextIndex)
    in roundRobinP' gens 0

eventuallyP :: forall a. (Arbitrary a) => StamageP a -> StamageP a
eventuallyP aStamageP = 
    Next $ do
        nPrepends <- elements [0..10] 
        let Next aGenStamage = applyN nPrepends (nextP arbitrary) aStamageP
        aGenStamage

orP :: StamageP a -> StamageP a -> StamageP a
orP firstStamage secondStamage = Next $ do
            Next aStamagePGen <- elements [firstStamage, secondStamage]
            aStamagePGen

suchThatP :: StamageP a -> TPred a -> StamageP a
suchThatP _ _ = error "Not implemented"

mkStamageP :: TPred a -> StamageP a
mkStamageP _ = error "Not implemented"--something very similar to evalLTL.



