{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module GeneratorTest (
    runTests
) where
import Generators
import Evaluators
import Helpers 
import Test.QuickCheck
import Rattus.Stream
import Rattus
import LTL
import Data.Bits ((.|.))

prop_alternatesEvenOdd :: Property
prop_alternatesEvenOdd =
    forAll evenOddGenP alternatesEvenOdd

prop_alternatesOddEven :: Property
prop_alternatesOddEven =
    forAll oddEvenGenP alternatesOddEven

prop_isUnique :: Property
prop_isUnique =
    forAll
        (stamagePGen (stamagePUnique:: StamageP Int))
        areUnique

evenOddGenPair :: [Gen Int]
evenOddGenPair = [
        (*2) <$> arbitrary,
        (.|. 1) <$> arbitrary
    ]

prop_roundRobinAltOddEven :: Property
prop_roundRobinAltOddEven =
    forAll
        (stamagePGen $ roundRobinP evenOddGenPair)
        alternatesEvenOdd

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
        $ Always $ hasHead (2::Int)

altsOddOrEven :: TPred Int
altsOddOrEven = Always $
                    (SP (even . strHead) `And` Imminently (SP (odd . strHead)))
                    `Or`
                    (SP (odd . strHead) `And` Imminently (SP (even . strHead)))
prop_suchThatP_evensOrOddssuchThatEven_Evens :: Property
prop_suchThatP_evensOrOddssuchThatEven_Evens =
    satisfies
        (evenOddP `orP` oddEvenP `suchThatP` SP (even . strHead))
        altsOddOrEven

prop_mkStamage_startTrue_startsTrue :: Property
prop_mkStamage_startTrue_startsTrue =
    satisfies
        (mkStamageP (hasHead True))
        $ hasHead True

prop_mkStamage_start101_starts101 :: Property
prop_mkStamage_start101_starts101 =
    satisfies
        (mkStamageP starts101)
        starts101
    where starts101 = hasHead (1::Int)
                        `And` Imminently (hasHead 0)
                        `And` Imminently (Imminently $ hasHead 1)

disabled_prop_mkStamage_evenOdd_isOddEven :: Property
disabled_prop_mkStamage_evenOdd_isOddEven =
    satisfies
        (mkStamageP altsOddOrEven)
        altsOddOrEven

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