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
newtype Stamage a = NextG (Gen (Maybe (a, Stamage a)))


getJust (Just a) = a
getJust Nothing = error "Runner should get Nothing"

stamageRun :: Stamage a -> Gen (Str a)
stamageRun (NextG aGen) = do
    aMaybe <- aGen
    let (value, aStamage) = getJust aMaybe
    rest <- stamageRun aStamage
    return $ value ::: delay rest

instance (Arbitrary a) => Arbitrary (Str a) where
    arbitrary = stamageRun arbitraryStamage

-- Stream Generators ----------------------------------------------------------

oddEvenGen :: Gen (Str Int)
oddEvenGen = stamageRun oddEven

constStrOf ::  a -> Gen (Str a)
constStrOf value = stamageRun $ constOfG value

--- Stamage producers ---------------------------------------------------------

arbitraryStamage :: forall a. (Arbitrary a) => Stamage a 
arbitraryStamage = NextG $ do 
    element <- (arbitrary :: Gen a)
    return (Just (element, arbitraryStamage))

stamageOfStr :: Str a -> Stamage a
stamageOfStr (h ::: t) = NextG $ return $ Just (h, stamageOfStr (adv t))

padList :: (Arbitrary a ) => [a] -> Stamage a
padList (h:t) = NextG $ return (Just (h, padList t))
padList [] = arbitraryStamage

evenOdd :: Stamage Int
evenOdd = NextG $ do
        value <- (*2) <$> (arbitrary:: Gen Int)
        return $ Just (value, oddEven)

oddEven :: Stamage Int
oddEven = NextG $ do
        value <- (.|. 1) <$> arbitrary
        return $ Just (value, evenOdd)

constOfG :: a -> Stamage a
constOfG value = NextG $ return (Just (value, constOfG value))
                       
