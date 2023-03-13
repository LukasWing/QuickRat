module StreamGenTest (
    testStreamGen
) where 
import StreamGen
import Test.QuickCheck 

prop_constStreamsAreEqual :: Int -> Bool
prop_constStreamsAreEqual v =
    constStr v =~= constStr v 

prop_headEqualIfAlmostEqual :: Str Int -> Str Int -> Property
prop_headEqualIfAlmostEqual as1 as2 =
        as1 =~= as2 ==>  strTake 10 as1 == strTake 10 as1
        
testStreamGen = do
    quickCheck prop_constStreamsAreEqual
    quickCheck prop_headEqualIfAlmostEqual