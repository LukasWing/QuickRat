module StreamGenTest (
    testStreamGen
)
where 
import StreamGen (constQStr, QStr, almostEqual)

import Test.QuickCheck  

prop_constStreamsAreEqual :: Int -> Bool
prop_constStreamsAreEqual v =
    constQStr v `almostEqual` constQStr v 



testStreamGen = do
    quickCheck prop_constStreamsAreEqual