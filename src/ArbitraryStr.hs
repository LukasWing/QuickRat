module ArbitraryStr (
    mainStr,
)
where 
import Test.QuickCheck
import Rattus.Stream (Str ((:::)))
import Rattus (delay)
import Rattus.Primitives (adv)
import StrUtils (getHead, constStr)

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

prop_intConstStreamsAreEqual :: Int -> Bool
prop_intConstStreamsAreEqual n =
    ArbitraryStr (constStr n) =~= ArbitraryStr (constStr n)

-- Gives up. Needs generator tuning.
prop_headEqualIfAlmostEqual :: ArbitraryStr -> ArbitraryStr -> Property
prop_headEqualIfAlmostEqual as1 as2 =
        as1 =~= as2 ==> arbitraryStrHead as1 == arbitraryStrHead as2
       

testAlmostEq :: IO ()
testAlmostEq = do
    print $ ArbitraryStr (constStr 3) =~= ArbitraryStr (constStr 3)
    quickCheck prop_intConstStreamsAreEqual
    quickCheck prop_headEqualIfAlmostEqual

mainStr :: IO ()
mainStr = do
    -- print $ getHead5 makeConstStr
    num <- generate (arbitrary::Gen ArbitraryStr)
    print num
    testAlmostEq    
    putStrLn "mainStr Done"