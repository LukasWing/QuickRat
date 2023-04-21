{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wmissing-export-lists #-}
module Generators where
import Helpers
import Rattus.Stream hiding (filter, const)
import Rattus
import Rattus.Primitives
import Test.QuickCheck hiding ((.&.))
import Data.Bits ( Bits((.&.), (.|.)) )
import Control.Monad.State
import qualified Data.Set as Set
import LTL
import Data.Map (valid)
import Text.Printf (errorBadArgument)
import Control.Exception (bracket, bracket_)
import GHC.IO.Exception (assertError)

-- Foundations -------------------------------------------------------------
type Stamage a = StamageP a
newtype StamageP a = Next (Gen (a, StamageP a))
stamagePGen :: StamageP a -> Gen (Str a)
stamagePGen (Next aGen) = do
    (value, aStamageP) <- aGen
    rest <- stamagePGen aStamageP
    return $ value ::: delay rest

getGen :: StamageP a -> Gen (a, StamageP a)
getGen (Next gen) = gen 

instance (Arbitrary a) => Arbitrary (Str a) where
    arbitrary = stamagePGen arbitraryStamageP

-- Stream Generators ----------------------------------------------------------
oddEvenGenP :: Gen (Str Int)
oddEvenGenP = stamagePGen oddEvenP

evenOddGenP :: Gen (Str Int)
evenOddGenP = stamagePGen evenOddP

uniqueStr :: (Arbitrary a, Ord a) => Gen (Str a)
uniqueStr = stamagePGen stamagePUnique

constStrOfP ::  a -> Gen (Str a)
constStrOfP value = stamagePGen $ stamagePOfStr $ constStr value

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
        getGen (applyN nPrepends (nextP tipGen) aStamageP)
        

afterP ::  forall a. (Arbitrary a) => Int -> StamageP a -> StamageP a
afterP anInt aStamageP =
    if anInt > 0 then Next $ do
        element <- arbitrary
        return (element, afterP (anInt - 1) aStamageP)
    else aStamageP

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
suchThatP aStamageP aFilter = Next $ do
    strs <- vectorize (stamagePGen aStamageP)
    let Next result =  
            case collapseP $ filter (evalLTL aFilter) strs of
                Just acc -> acc
                Nothing -> error "No matching streams generated"
    result

    where
        vectorize :: Gen a -> Gen [a]
        vectorize gen = sized $ \n -> vectorOf ((n+1)*100) gen

        collapseP :: [Str a] -> Maybe (StamageP a)
        collapseP streams = if not $ null streams
            then Just $ Next $ do
                aStr <- elements streams
                let Next asg = stamagePOfStr aStr
                asg
            else Nothing


--- StamageP generators -----------------------------------------------------

arbitraryStamageP :: (Arbitrary a) => StamageP a
arbitraryStamageP = Next $ do
    element <- arbitrary
    return (element, arbitraryStamageP)

stamagePUnique :: (Arbitrary a, Ord a) => StamageP a
stamagePUnique = stamagePUnique' Set.empty where
    stamagePUnique' :: (Arbitrary a, Ord a) => Set.Set a -> StamageP a
    stamagePUnique' acc = Next $ do
        element <- arbitrary `suchThat` (`Set.notMember` acc)
        let newSet = Set.insert element acc
        return (element, stamagePUnique' newSet)

stamagePOfStr :: Str a -> StamageP a
stamagePOfStr (h ::: t) = Next $ return (h, stamagePOfStr (adv t))

padListP :: (Arbitrary a ) => [a] -> StamageP a
padListP (h:t) = Next $ return (h, padListP t)
padListP [] = arbitraryStamageP

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

mkStamageP :: (Arbitrary a) => TPred a -> StamageP a
mkStamageP formulae = case formulae of
    SP aStrPred     -> arbitraryStamageP `suchThatP` SP aStrPred
    Not aTPred      -> errorNotImplemented
    Or phi psi      -> mkStamageP phi `orP` mkStamageP psi
    And phi psi     -> errorNotImplemented
    Implies phi psi -> errorNotImplemented
    Imminently phi  -> nextP arbitrary $ mkStamageP phi 
    Eventually phi  -> eventuallyP $ mkStamageP phi
    Until phi psi   -> errorNotImplemented
    Always phi      -> errorNotImplemented
    After anInt phi -> afterP anInt $ mkStamageP phi
                       
                       
 