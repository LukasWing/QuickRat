{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module EvaluatorTest (
    runTests
) where
import Generators hiding (next)
import Evaluators
import Helpers (constStr)
import Test.QuickCheck
import Rattus.Stream


prop_seesConst :: Int -> Bool
prop_seesConst value = isConstCheck (constStr (value::Int))

return []
runTests :: IO Bool
runTests = $quickCheckAll




