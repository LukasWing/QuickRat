{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module StreamGen (
    strTake,
    Str,
    constStr,
    strHead,
    AlmostEq(..),
    evenOddGen,
    oddEven,
    evenOdd
)
where
import Rattus.Stream
import Rattus
import Rattus.Primitives
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen(MkGen))

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

evenOddGen :: Stamage Int -> Str Int
evenOddGen aStamage = gen aStamage ::: delay (evenOddGen (next aStamage (0:: Int)))

data Stamage a = Stamage {
    gen::a,
    next:: a -> Stamage a
}

oddEven = Stamage {gen = 1::Int, next = \_ -> evenOdd}
evenOdd = Stamage {gen = 2::Int, next = \_ -> oddEven}




