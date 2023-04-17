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
-- stamateIsAlternating :: Stamate Int -> Bool
-- stamateIsAlternating aStamate = stamateRunStamage aStamate isAlternatingSM

prop_next_EvenOddPrependendEven_nextEvenOddHolds :: Property
prop_next_EvenOddPrependendEven_nextEvenOddHolds =
    forAll 
        (stamageGen (next (return 2) evenOdd))
        $ \(h ::: orig) -> alternatesEvenOdd (adv orig) && h == 2 


return []
runTests :: IO Bool
runTests = $quickCheckAll




