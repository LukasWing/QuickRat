{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module StreamGen where
import Rattus.Stream
import Rattus
import Rattus.Primitives
import Test.QuickCheck hiding ((.&.))
import Data.Bits ( Bits((.&.), (.|.)) )
import Control.Monad.State
import qualified Data.Set as Set
import Data.Data (Data)


class AlmostEq a where
    (=~=) :: a -> a -> Bool



instance (Arbitrary a) => Arbitrary (Str a) where
    arbitrary = do
        x <- arbitrary::Gen a
        xs <- (arbitrary::Gen (Str a))
        return $ x:::delay xs


instance (Show a) => Show (Str a) where
    show = show . strTake 20

instance (Eq a) => AlmostEq (Str a) where
    stream1 =~= stream2 = strTake 5 stream1 == strTake 5 stream2

strTake :: Integer -> Str a -> [a]
strTake n aStr =
    let strTake' picksLeft accumulator (h:::t) =
            if picksLeft > 0
                then strTake' (picksLeft - 1) (h:accumulator) (adv t)
                else reverse accumulator
    in strTake' n [] aStr

strHead :: Str a -> a
strHead  (h:::_) = h

constStr :: a -> Str a
constStr v = v ::: delay (constStr v)

data Stamage a = Stamage {
    gen:: Gen a,
    next:: a -> Stamage a
}

stamageGen aStamage = do
    v <- gen aStamage
    t <- stamageGen (next aStamage v)
    return $ v ::: delay t

oddGen :: Gen Int
oddGen = (.|. 1) <$> arbitrary

evenGen :: Gen Int
evenGen = (*2) <$> arbitrary

oddEven = Stamage {gen = oddGen, next = \_ -> evenOdd}
evenOdd = Stamage {gen = evenGen, next = \_ -> oddEven}

oddEvenGen = stamageGen oddEven
evenOddGen = stamageGen evenOdd

increasingSM :: Int -> Stamage Int
increasingSM current = Stamage {
    gen = do
        addend <- arbitrary
        return (current + abs addend),
    next = \prev' -> increasingSM (prev' + current)
}

increasingInts :: Gen (Str Int)
increasingInts = stamageGen (increasingSM 0)

uniqueIntStr :: Gen (Str Int)
uniqueIntStr = stamageGen (uniqueSM (Set.empty :: Set.Set Int))

uniqueSM ::  Set.Set Int -> Stamage Int
uniqueSM acc = Stamage {
    gen = suchThat (arbitrary::Gen Int) (\n -> not (Set.member n acc)),
    next = \newInt -> uniqueSM $ Set.insert newInt acc
}


run = do
    print "inc ints"
    sample increasingInts
    sample uniqueIntStr






