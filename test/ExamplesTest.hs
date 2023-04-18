{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module ExamplesTest (
    runTests
) where
import Generators hiding (next)
import Evaluators hiding (next)
import Helpers
import Test.QuickCheck
import Examples
import Rattus.Stream
import Rattus
import LTL

prop_next_EvenOddPrependendEven_nextEvenOddHolds :: Property
prop_next_EvenOddPrependendEven_nextEvenOddHolds =
    forAll
        (stamageGen (next (return 2) evenOdd))
        $ \(h ::: orig) -> alternatesEvenOdd (adv orig) && h == 2

threeUntilEvenOdd :: TPred Int
threeUntilEvenOdd = SP ((==3) . strHead) `Until` SP alternatesEvenOdd

prop_until_threeUntilEvenOdd :: Property
prop_until_threeUntilEvenOdd =
    forAll
        (stamageGen (Examples.until (return (3::Int)) 5 evenOdd))
        $ evalLTL threeUntilEvenOdd

prop_until_anyIntUntilSomeStr :: Property
prop_until_anyIntUntilSomeStr =
    forAll
        (stamageGen (Examples.until (choose (minVal, maxVal)) 5 evenOdd))
        $ evalLTL $ SP (inBounds . strHead) `Until` SP alternatesEvenOdd
    where minVal = -10
          maxVal = 10  
          inBounds i = i >= minVal && i <= maxVal

prop_eventually_randomToEvenOdd_LTLFits :: Property
prop_eventually_randomToEvenOdd_LTLFits = 
    forAll 
        (stamageGen (Examples.eventually 5 evenOdd))
        $ evalLTL $ Eventually $ SP alternatesEvenOdd

prop_roundRobin_Gen1AndGen2_OutPutIsEvenOdd :: Property 
prop_roundRobin_Gen1AndGen2_OutPutIsEvenOdd =
    forAll
        (stamageGen (Examples.roundRobin [inputEven, inputOdd]))
        $ evalLTL $ SP alternatesEvenOdd
    where   inputEven = fmap (*(2::Int)) arbitrary
            inputOdd  = fmap (\i -> i * 2 + 1) arbitrary  
          
prop_or_OorK_OorKisOkay :: Property
prop_or_OorK_OorKisOkay =  
    forAll 
        (stamageGen (Examples.or True (constOf 'O') (constOf 'K')))
        $ evalLTL $ SP ((=='O') . strHead) `Or` SP ((=='K') . strHead)

return []
runTests :: IO Bool
runTests = $quickCheckAll




