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
    forAll evenOddGen $ \aStr -> alternatesEvenOdd aStr 100

prop_alternatesOddEven :: Property
prop_alternatesOddEven =
    forAll oddEvenGen $ \aStr -> alternatesOddEven aStr 100

prop_isIncreasing :: Property
prop_isIncreasing = forAll (increasingNums::Gen (Str Float)) areIncreasing

prop_isUnique :: Property
prop_isUnique = forAll (uniqueStr::Gen (Str (Int, Float))) areUnique

prop_isConst :: Property
prop_isConst = forAll (constStrSM::Gen (Str (Int, Bool))) isConstCheck

return []
runTests :: IO Bool
runTests = $quickCheckAll




