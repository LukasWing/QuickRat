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
import Types
import Functions

-- Foundations -------------------------------------------------------------
instance (Arbitrary a) => Arbitrary (Str a) where
    arbitrary = stamageRun arbitraryStamage


-- isStamageContradiction (NextG (Just _)) = False

-- Stream Generators ----------------------------------------------------------

oddEvenGen :: Gen (Str Int)
oddEvenGen = stamageRun oddEven

constStrOf ::  a -> Gen (Str a)
constStrOf value = stamageRun $ constOfG value

--- Stamage producers ---------------------------------------------------------
stamageOfStr :: Str a -> Stamage a
stamageOfStr (h ::: t) = NextG $ return $ Just (h, stamageOfStr (adv t))

padList :: (Arbitrary a ) => [a] -> Stamage a
padList (h:t) = NextG $ return (Just (h, padList t))
padList [] = arbitraryStamage

oddGen :: Gen Int
oddGen = (.|.) 1 <$> arbitrary

evenGen :: Gen Int
evenGen = (2*) <$> arbitrary

evenOdd :: Stamage Int
evenOdd = NextG $ do
        value <- evenGen
        return $ Just (value, oddEven)

oddEven :: Stamage Int
oddEven = NextG $ do
        value <- oddGen
        return $ Just (value, evenOdd)

constOfG :: a -> Stamage a
constOfG value = NextG $ return (Just (value, constOfG value))
