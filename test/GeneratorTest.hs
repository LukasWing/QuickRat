{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module GeneratorTest (
    runTests
) where
import Generators hiding (next)
import Evaluators
import Helpers (strGet, strHead, strTake)
import Test.QuickCheck
import Rattus.Stream

prop_alternatesEvenOdd :: Property
prop_alternatesEvenOdd =
    forAll evenOddGen alternatesEvenOdd

prop_alternatesOddEven :: Property
prop_alternatesOddEven =
    forAll oddEvenGen alternatesOddEven

prop_isIncreasing :: Property
prop_isIncreasing = forAll (increasingNums::Gen (Str Float)) areIncreasing

prop_isUnique :: Property
prop_isUnique = forAll (uniqueStr::Gen (Str (Int, Float))) areUnique

prop_isConst :: Property
prop_isConst = forAll (constStrSM::Gen (Str (Int, Bool))) isConstCheck

prop_roundRobin01 :: Property
prop_roundRobin01 = forAll (roundRobin [return (0::Int), return 1]) $ alternatesAB (0, 1)

prop_roundRobinConstHey :: Property
prop_roundRobinConstHey = forAll (roundRobin [return "hey"]) isConstCheck


evenOddGenPair :: [Gen Int]
evenOddGenPair = [ 
        oneof $ Prelude.map return [-1000, -998 .. 1000], 
        oneof $ Prelude.map return [-999, -997 .. 999]
    ]

prop_roundRobinAltOddEven :: Property
prop_roundRobinAltOddEven = 
    forAll 
        (roundRobin evenOddGenPair) 
        alternatesEvenOdd

prop_prependStamage_anyAppend_sameOut :: Property
prop_prependStamage_anyAppend_sameOut = 
    forAll 
        (stamageGen $ prependStamage (constOf (2::Int)) 3)
        $ \aStr -> strTake 5 aStr  == [3, 2, 2, 2, 2];  

prop_prependNStamage_fixedAppend_listAppended :: Property
prop_prependNStamage_fixedAppend_listAppended = 
    forAll 
        (stamageGen $ prependNStamage (arbitraryStamage :: Stamage Int) [3, 3])
        $ \aStr -> strTake 2 aStr  == [3, 3];
        
nonChatty :: Args
nonChatty = stdArgs {chatty = True}

displayOnlyFailing :: Property -> IO Result
displayOnlyFailing aProperty = do
    result <- quickCheckWithResult nonChatty aProperty
    case result of
        Success {} -> return result
        _ ->  quickCheckResult aProperty


return []
runTests :: IO Bool
runTests = $forAllProperties displayOnlyFailing