{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module StreamGenTest (
    runTests
) where
import StreamGen 
import Test.QuickCheck
import Rattus.Stream
import Rattus

prop_constStreamsAreEqual :: Int -> Bool
prop_constStreamsAreEqual v =
    constStr v =~= constStr v

threesGen :: Gen (Str Int, Str Int)
threesGen = scale (`mod` 2) (arbitrary :: Gen (Str Int, Str Int))

propDisabledHeadEqualIfAlmostEqualVerbose :: Property
propDisabledHeadEqualIfAlmostEqualVerbose =
    forAll threesGen $ \(as1, as2) ->
        collect (show as1 ++ show as2) $ not (as1 =~= as2) || (strTake 2 as1 == strTake 2 as2)

prop_headEqualIfAlmostEqual :: Property
prop_headEqualIfAlmostEqual =
    forAll threesGen $ \(as1, as2) ->
        as1 =~= as2 ==> strTake 2 as1 == strTake 2 as2

prop_headNotEqual_notAlmostEqual :: Str Int -> Str Int -> Property
prop_headNotEqual_notAlmostEqual as1 as2 =
    strTake 2 as1 /= strTake 2 as2 ==> not (as1 =~= as2)

prop_alternatesEvenOdd :: Property
prop_alternatesEvenOdd =
    forAll evenOddGen $ \aStr -> alternatesEvenOdd aStr 100

prop_alternatesOddEven :: Property
prop_alternatesOddEven = 
    forAll oddEvenGen $ \aStr -> alternatesOddEven aStr 100

alternatesEvenOdd :: Str Int -> Int -> Bool
alternatesEvenOdd (h ::: t) checksLeft =
    checksLeft == 0
    || even h 
    && alternatesOddEven (adv t) (checksLeft - 1)

alternatesOddEven :: Str Int -> Int -> Bool
alternatesOddEven (h ::: t) checksLeft =
    checksLeft == 0
    || odd h 
    && alternatesEvenOdd (adv t) checksLeft


return []
runTests :: IO Bool
runTests = $quickCheckAll



