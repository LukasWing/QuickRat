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
import Evaluators (stamateRunStamage)

stamateIsAlternating :: Stamate Int -> Bool
stamateIsAlternating aStamate = stamateRunStamage aStamate isAlternatingSM

prop_next_EvenOddPrependendEven_nextEvenOddHolds :: Property
prop_next_EvenOddPrependendEven_nextEvenOddHolds =
    forAll 
        (return (2::Int))
        $ \gen ->   let actual = next (return gen) evenOdd
                    in isAlternatingSM True

return []
runTests :: IO Bool
runTests = $quickCheckAll




