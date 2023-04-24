{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module EvaluatorTest (
    runTests
) where
import Generators hiding (next)
import Evaluators
import Helpers
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Rattus.Stream hiding (const)


prop_seesConst :: Int -> Bool
prop_seesConst value = isConstCheck (constStr (value::Int))

prop_strProbEqSameStrTrue :: Eq a => Str a -> Bool
prop_strProbEqSameStrTrue aStr = aStr `strProbEq` aStr

prop_stamateRun'_anyStream_constsOK :: Str Bool -> NonNegative Int -> Bool
prop_stamateRun'_anyStream_constsOK aStr (NonNegative {getNonNegative=i}) =
    not (stamateRun' aStr Fail 0)
    && (stamateRun' aStr (NextT (const Fail)) 0)
    && stamateRun' aStr Pass 0
    && stamateRun' aStr (NextT (const Pass)) i

prop_stamateRun'_extendedStreams_constsOK :: Bool
prop_stamateRun'_extendedStreams_constsOK =
    stamateRun' (strExtend [1]) (isConstSM (Just 2)) 0
    && stamateRun' (strExtend [2,1,1,1,1]) (isConstSM (Just 2)) 1
    && stamateRun' (strExtend [2,2,1,1,1]) (isConstSM (Just 2)) 2
    && not (stamateRun' (strExtend [2,2,1,1,1]) (isConstSM (Just 1)) 2)
    && not (stamateRun' (strExtend [2,2,1,1,1]) (isConstSM (Just 2)) 3)

return []
runTests :: IO Bool
runTests = $quickCheckAll




