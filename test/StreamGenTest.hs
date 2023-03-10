module StreamGenTest (
    testStreamGen
)
where 
import StreamGen

import Test.QuickCheck  

prop_constStreamsAreEqual :: Int -> Bool
prop_constStreamsAreEqual v =
    constQStr v =~= constQStr v 

prop_headEqualIfAlmostEqual :: QStr Int -> QStr Int -> Property
prop_headEqualIfAlmostEqual as1 as2 =
        as1 =~= as2 ==>  qStrTake 10 as1 == qStrTake 10 as1
        
testStreamGen = do
    quickCheck prop_constStreamsAreEqual
    quickCheck prop_headEqualIfAlmostEqual