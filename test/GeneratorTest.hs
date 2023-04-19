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
import Control.Applicative (liftA)
import Data.Bits

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




hasHead :: (Eq p) => p -> TPred p
hasHead expectedHead = SP ((==expectedHead) . strHead)

satisfies :: Show a => StamageP a -> TPred a -> Property
aStamage `satisfies` anLTL =
    forAll
        (stamagePGen aStamage)
        $ evalLTL anLTL

prop_next_EvenOddPrependendEven_nextEvenOddHolds :: Property
prop_next_EvenOddPrependendEven_nextEvenOddHolds =
    forAll
        (stamagePGen (nextP (return 2) evenOddP))
        $ \(h ::: orig) -> alternatesEvenOdd (adv orig) && h == 2

threeUntilEvenOdd :: TPred Int
threeUntilEvenOdd = hasHead (3::Int) `Until` SP alternatesEvenOdd

prop_untilP_threeUntilEvenOdd :: Property
prop_untilP_threeUntilEvenOdd =
    return (3::Int) `untilP` evenOddP `satisfies` threeUntilEvenOdd


prop_untilP_anyIntUntilSomeStr :: Property
prop_untilP_anyIntUntilSomeStr =
    satisfies
       (choose (minVal, maxVal) `untilP` evenOddP)
       $ SP (inBounds . strHead) `Until` SP alternatesEvenOdd
    where minVal = -10
          maxVal = 10
          inBounds i = i >= minVal && i <= maxVal

prop_eventuallyP_randomToEvenOdd_LTLFits :: Property
prop_eventuallyP_randomToEvenOdd_LTLFits =
    satisfies
        (eventuallyP evenOddP)
        $ Eventually $ SP alternatesEvenOdd

prop_roundRobinP_Gen1AndGen2_OutPutIsEvenOdd :: Property
prop_roundRobinP_Gen1AndGen2_OutPutIsEvenOdd =
    satisfies
        (roundRobinP $ sequenceA [fmap (*(2::Int)), fmap (.|. 1)] arbitrary)
        $ SP alternatesEvenOdd

prop_orP_OorK_OorKisOkay :: Property
prop_orP_OorK_OorKisOkay =
    satisfies
        (constOfP 'O' `orP` constOfP 'K')
        $ hasHead 'O' `Or` hasHead 'K'

prop_suchThatP_1111or2222suchThatEven_2222 :: Property
prop_suchThatP_1111or2222suchThatEven_2222 =
    satisfies
        (constOfP 1 `orP` constOfP 2 `suchThatP` SP (even . strHead))
        $ Always $ hasHead (1::Int)

prop_suchThatP_evensOrOddssuchThatEven_Evens :: Property
prop_suchThatP_evensOrOddssuchThatEven_Evens =
    satisfies
        (constOfP 1 `orP` constOfP 2 `suchThatP` SP (even . strHead))
        $ Always $ hasHead (1::Int)

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