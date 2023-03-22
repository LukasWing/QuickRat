{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module EvaluatorTest (
    runTests
) where
import Generators hiding (next)
import Evaluators
import Helpers
import Test.QuickCheck
import Rattus.Stream

prop_seesAlternatingEvenOdd :: Property
prop_seesAlternatingEvenOdd =
    forAll evenOddGen alternatesEvenOdd 

prop_seesConst :: Int -> Bool
prop_seesConst value = isConstCheck (constStr (value::Int))

return []
runTests :: IO Bool
runTests = $quickCheckAll




