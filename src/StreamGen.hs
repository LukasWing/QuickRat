{-# LANGUAGE ScopedTypeVariables #-}
module StreamGen (
    qStrTake,
    QStr,
    constQStr,
    qStrHead,
    AlmostEq(..),
)
where
import Rattus.Stream (Str ((:::)))
import Rattus (delay)
import Rattus.Primitives (adv)
import Test.QuickCheck

newtype QStr a = QStr {unQStr::Str a}

class AlmostEq a where
    (=~=) :: a -> a -> Bool


instance (Arbitrary a) => Arbitrary (QStr a) where
    arbitrary = do
        x <- arbitrary::Gen a
        xs <- (arbitrary::Gen (QStr a))
        return $ QStr (x:::delay (unQStr xs))

instance (Show a) => Show (QStr a) where
    show = show . qStrTake 5

instance (Eq a) => AlmostEq (QStr a) where
    stream1 =~= stream2 = qStrTake 100 stream1 == qStrTake 100 stream2

--fix for above:
almostEqual :: Eq a => QStr a -> QStr a -> Bool
stream1 `almostEqual` stream2 = qStrTake 100 stream1 == qStrTake 100 stream2

qStrTake :: Integer -> QStr a -> [a]
qStrTake n (QStr aStr) = 
        let qStrTake' picksLeft accumulator (h:::t) =
                if picksLeft > 0 
                    then qStrTake' (picksLeft - 1) (h:accumulator) (adv t) 
                    else reverse accumulator
        in qStrTake' n [] aStr

qStrHead :: QStr a -> a
qStrHead aQStr = getHead' aStr
        where 
            aStr = unQStr aQStr
            getHead' (h:::_) = h

constQStr :: a -> QStr a
constQStr v = QStr $ v ::: delay ( unQStr (constQStr v))