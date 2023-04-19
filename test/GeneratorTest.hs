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
import Rattus
import LTL

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

prop_next_EvenOddPrependendEven_nextEvenOddHolds :: Property
prop_next_EvenOddPrependendEven_nextEvenOddHolds =
    forAll
        (stamagePGen (nextP (return 2) evenOddP))
        $ \(h ::: orig) -> alternatesEvenOdd (adv orig) && h == 2

threeUntilEvenOdd :: TPred Int
threeUntilEvenOdd = SP ((==3) . strHead) `Until` SP alternatesEvenOdd

prop_untilP_threeUntilEvenOdd :: Property
prop_untilP_threeUntilEvenOdd =
    forAll
        (stamagePGen (untilP (return (3::Int))  evenOddP))
        $ evalLTL threeUntilEvenOdd

prop_untilP_anyIntUntilSomeStr :: Property
prop_untilP_anyIntUntilSomeStr =
    forAll
        (stamagePGen (untilP (choose (minVal, maxVal)) evenOddP))
        $ evalLTL $ SP (inBounds . strHead) `Until` SP alternatesEvenOdd
    where minVal = -10
          maxVal = 10  
          inBounds i = i >= minVal && i <= maxVal


prop_eventuallyP_randomToEvenOdd_LTLFits :: Property
prop_eventuallyP_randomToEvenOdd_LTLFits = 
    forAll 
        (stamagePGen (eventuallyP evenOddP))
        $ evalLTL $ Eventually $ SP alternatesEvenOdd

prop_roundRobinP_Gen1AndGen2_OutPutIsEvenOdd :: Property 
prop_roundRobinP_Gen1AndGen2_OutPutIsEvenOdd =
    forAll
        (stamagePGen (roundRobinP [inputEven, inputOdd]))
        $ evalLTL $ SP alternatesEvenOdd
    where   inputEven = fmap (*(2::Int)) arbitrary
            inputOdd  = fmap (\i -> i * 2 + 1) arbitrary  
          
prop_orP_OorK_OorKisOkay :: Property
prop_orP_OorK_OorKisOkay =  
    forAll 
        (stamagePGen (orP (constOfP 'O') (constOfP 'K')))
        $ evalLTL $ SP ((=='O') . strHead) `Or` SP ((=='K') . strHead)

displayOnlyFailing :: Property -> IO Result
displayOnlyFailing aProperty = do
    result <- quickCheckWithResult nonChatty aProperty
    case result of
        Success {} -> return result
        _ ->  quickCheckResult aProperty


return []
runTests :: IO Bool
runTests = $forAllProperties displayOnlyFailing