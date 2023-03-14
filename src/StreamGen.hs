{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module StreamGen (
    strTake,
    Str,
    constStr,
    strHead,
    AlmostEq(..),
    evenOddGen,
    oddEvenGen,
    oddEven,
    evenOdd

)
where
import Rattus.Stream
import Rattus
import Rattus.Primitives
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen (Gen(MkGen))
import Data.Bits ( Bits((.&.), (.|.)) )

class AlmostEq a where
    (=~=) :: a -> a -> Bool

instance (Arbitrary a) => Arbitrary (Str a) where
    arbitrary = do
        x <- arbitrary::Gen a
        xs <- (arbitrary::Gen (Str a))
        return $ x:::delay xs

instance (Show a) => Show (Str a) where
    show = show . strTake 5

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


-- class OddEvenGen where
--     evenOddGen:: Int -> Str Int
-- oddEvenGen = arbitrary :: Gen (Str Int)

stamageGen aStamage = do
    v <- gen aStamage
    t <- stamageGen (next aStamage (0::Int))
    return $ v ::: delay t

data Stamage a = Stamage {
    gen::Gen a,
    next:: a -> Stamage a
}

oddGen = do 
    x <- arbitrary::Gen Int 
    return (x .|. 1)

evenGen = do 
    x <- arbitrary::Gen Int 
    return $ x * 2

oddEven = Stamage {gen = oddGen, next = \_ -> evenOdd}
evenOdd = Stamage {gen = evenGen, next = \_ -> oddEven}

oddEvenGen = stamageGen oddEven
evenOddGen = stamageGen evenOdd





