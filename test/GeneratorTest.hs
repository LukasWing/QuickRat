{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module GeneratorTest (
    runTests
) where
import Generators hiding (next)
import Evaluators
import Helpers ()
import Test.QuickCheck
import Rattus.Stream

prop_alternatesEvenOdd :: Property
prop_alternatesEvenOdd =
    forAll evenOddGen $ \aStr -> alternatesEvenOdd aStr

prop_alternatesOddEven :: Property
prop_alternatesOddEven =
    forAll oddEvenGen $ \aStr -> alternatesOddEven aStr

prop_isIncreasing :: Property
prop_isIncreasing = forAll (increasingNums::Gen (Str Float)) areIncreasing

prop_isUnique :: Property
prop_isUnique = forAll (uniqueStr::Gen (Str (Int, Float))) areUnique

prop_isConst :: Property
prop_isConst = forAll (constStrSM::Gen (Str (Int, Bool))) isConstCheck

prop_roundRobin :: Property
prop_roundRobin = forAll (roundRobin [return 0::Gen Int, return 1::Gen Int]) $ \value -> value == 2 || value == 1


nonChatty :: Args
nonChatty = Args {
    chatty = False,
    replay = replay stdArgs,
    maxSuccess = maxSuccess stdArgs,
    maxDiscardRatio = maxDiscardRatio stdArgs,
    maxSize = maxSize stdArgs,
    maxShrinks = maxShrinks stdArgs
}


displayOnlyFailing :: Property -> IO Result
displayOnlyFailing aProperty = do
    result <- quickCheckWithResult nonChatty aProperty
    case result of
        Success {} -> return result
        _ ->  quickCheckResult aProperty


return []
runTests :: IO Bool
runTests = $forAllProperties displayOnlyFailing





