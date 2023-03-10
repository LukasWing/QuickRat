module StrLib (mainStr, getHead, constStr) where

import Test.QuickCheck
import Rattus.Stream (Str ((:::)))
import Rattus (delay)
import Rattus.Primitives (adv)
import Test.QuickCheck.Test (test)

constStr :: t -> Str t
constStr v = v ::: delay (constStr v)

getHead ::Str a -> a
getHead (h:::_) = h


strTake :: (Ord t, Num t) => t -> Str a -> [a]
strTake n = strTake' n [] 
        where strTake' picksLeft accumulator (head:::tail) =
                if picksLeft > 0 
                    then strTake' (picksLeft - 1) (head:accumulator) (adv tail) 
                    else reverse accumulator

showStr :: Show a => Str a -> String
showStr aStr = show $ strTake 5 aStr

makeConstStr :: Str Int
makeConstStr = 5 ::: delay makeConstStr

makeMyConstStr :: Int -> Str Int
makeMyConstStr n = n ::: delay (makeMyConstStr n)

getHead5Int :: ArbitraryStr -> [Int]
getHead5Int = getHead5Int' 5 []
        where getHead5Int' picksLeft accumulator anArbStr =
                let (h1:::t1) = unArbitraryStr anArbStr in
                let rest = ArbitraryStr (adv t1) in
                if picksLeft > 0
                    then getHead5Int' (picksLeft - 1) (h1:accumulator) rest
                    else reverse accumulator
                    
arbitraryStrHead :: ArbitraryStr -> Int
arbitraryStrHead as = getHead $ unArbitraryStr as
newtype ArbitraryStr = ArbitraryStr {unArbitraryStr :: Str Int}


instance Arbitrary ArbitraryStr where
    arbitrary = do
        x <- arbitrary:: Gen Int
        xs <- arbitrary:: Gen ArbitraryStr
        return $ ArbitraryStr (x:::delay (unArbitraryStr xs))

instance Show ArbitraryStr where
    show = show . getHead5Int


class AlmostEq a where
    (=~=) :: a -> a -> Bool

instance AlmostEq ArbitraryStr where
    stream1 =~= stream2 = getHead5Int stream1 == getHead5Int stream2

prop_intConstStreamsAreEqual :: Int -> Bool
prop_intConstStreamsAreEqual n =
    ArbitraryStr (makeMyConstStr n) =~= ArbitraryStr (makeMyConstStr n)

-- Gives up. Needs generator tuning.
prop_headEqualIfAlmostEqual :: ArbitraryStr -> ArbitraryStr -> Property
prop_headEqualIfAlmostEqual as1 as2 =
        as1 =~= as2 ==> arbitraryStrHead as1 == arbitraryStrHead as2
       

testAlmostEq :: IO ()
testAlmostEq = do
    print $ ArbitraryStr makeConstStr =~= ArbitraryStr makeConstStr
    quickCheck prop_intConstStreamsAreEqual
    quickCheck prop_headEqualIfAlmostEqual

mainStr :: IO ()
mainStr = do
    -- print $ getHead5 makeConstStr
    num <- generate (arbitrary::Gen ArbitraryStr)
    print num
    testAlmostEq    
    putStrLn "mainStr Done"




