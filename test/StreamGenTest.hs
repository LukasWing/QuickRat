{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module StreamGenTest (
    runTests
) where
import StreamGen hiding (next)
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

prop_isIncreasing :: Property
prop_isIncreasing = forAll (increasingNums:: Gen (Str Float)) areIncreasing

areIncreasing :: (Ord a) => Str a -> Bool
areIncreasing (h1 ::: t1) =
    let isIncreasing e1 (e2 ::: t2) checksLeft =
            checksLeft == 0
            || (e1 <= e2)
            && isIncreasing e2 (adv t2) (checksLeft - 1)
        checks = (20::Int)
    in isIncreasing h1 (adv t1) checks

prop_isUnique :: Property
prop_isUnique = forAll (uniqueStr::Gen (Str String)) areUnique

-- Todo
areUnique :: Str a -> Bool
areUnique _ = True


data Stamate a = Stamate {
    check:: a -> Bool,
    next:: a -> Stamate a
}

stamateRun :: Str a -> Stamate a -> Bool
stamateRun aStr aStamate =
    let stamateRun' (h:::t) aStamate' checksLeft =
            checksLeft == 0
            || check aStamate h
            && stamateRun' (adv t) (next aStamate' h) (pred checksLeft)
    in stamateRun' aStr aStamate 20


isConstSM :: Eq a => Maybe a -> Stamate a
isConstSM input = Stamate {
    check = \value -> case input of
                        Just val' -> val' == value
                        Nothing -> True,
    next = isConstSM . Just
}


isConstCheck :: (Eq a ) => Str a -> Bool
isConstCheck aStr = stamateRun aStr (isConstSM Nothing)


prop_isConst :: Property
prop_isConst = forAll (constStrSM::Gen (Str (Int, Bool))) isConstCheck


return []
runTests :: IO Bool
runTests = $quickCheckAll




