{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module PlayArea where
import Test.QuickCheck
import Control.Monad.Writer
    ( MonadWriter(writer), runWriter, Writer )
import Data.Char
import System.Random
import Control.Monad.State
import Rattus.Primitives
import Debug.Trace
import Rattus.Stream hiding (filter, map)
import Test.QuickCheck.Monadic
import qualified Test.QuickCheck.Monadic as M
import Data.Bits ((.|.))

genComplex'' = arbitrary >>= (\a ->
             arbitrary >>= (\b ->
             return Z {a=a, b=b}))
data Z = Z {a :: Float, b :: Float}

genComplex = arbitrary >>= (\a -> arbitrary >>= (\b -> return Z {a = a, b = b}))

genComplex' = do
  a <- arbitrary
  b <- arbitrary
  return Z {a = a, b = b}


rollDice :: Int -> Gen [Int]
rollDice n = replicateM n (arbitrary >>= (\i -> return (i `mod` 6 + 1)))

-- prop_1 :: (Ord a1, Num a1) => NonNegative Int -> Int -> Bool
prop_1 :: NonNegative Int -> Bool
prop_1 (NonNegative n) = (n::Int) > (-1)

prop_2 :: InfiniteList (NonNegative Int) -> Large Int -> Bool
prop_2 (InfiniteList (xs :: [NonNegative Int]) _) (Large n) = getNonNegative (xs !! n) > ((-1)::Int)


m2 :: IO Int -> IO Int
m2 = fmap (2*)
m2' :: Int -> Int
m2' = ((2::Int)*)

prop_monadic :: IO Int -> Property
prop_monadic a = monadicIO $ do
    a' <- Test.QuickCheck.Monadic.run (m2 a)
    assert (a' == 4)

f1' :: IO Int -> IO Int
f1' = fmap (+1)
prop_f :: Property
prop_f = monadicIO $ do
  x <- M.run (return 1)
  y <- M.run (f1' (return x))
  assert (y == x + 1)

diceRoller :: Int -> Gen Int
diceRoller nSides = do
        i <- (arbitrary:: Gen Int)
        return (i `mod` nSides + 1)

genMe :: IO Int
genMe = generate (arbitrary :: Gen Int)


run = do
   
    print "Done"