{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- your code here

module EvaluatorTest (
    runTests
) where
import Generators hiding (next)
import Evaluators
import Helpers
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Rattus.Stream hiding (const)
import Test.QuickCheck.Monadic
import Data.Maybe (isNothing)
import LTL (tautology, alternatesOddEven)
import Evaluators (mkStamage)


prop_seesConst :: Int -> Bool
prop_seesConst value = isConstCheck (constStr (value::Int))

prop_strProbEqSameStrTrue :: Eq a => Str a -> Bool
prop_strProbEqSameStrTrue aStr = aStr `strProbEq` aStr

prop_stamateRun'_anyStream_constsOK :: Str Bool -> NonNegative Int -> Bool
prop_stamateRun'_anyStream_constsOK aStr (NonNegative {getNonNegative=i}) =
    not (stamateRun' aStr Fail 0)
    && stamateRun' aStr (NextT (const Fail)) 0
    && stamateRun' aStr Pass 0
    && stamateRun' aStr (NextT (const Pass)) i

prop_stamateRun'_extendedStreams_constsOK :: Bool
prop_stamateRun'_extendedStreams_constsOK =
    stamateRun' (strExtend [1]) (isConstSM (Just 2)) 0
    && stamateRun' (strExtend [2,1,1,1,1]) (isConstSM (Just 2)) 1
    && stamateRun' (strExtend [2,2,1,1,1]) (isConstSM (Just 2)) 2
    && not (stamateRun' (strExtend [2,2,1,1,1]) (isConstSM (Just 1)) 2)
    && not (stamateRun' (strExtend [2,2,1,1,1]) (isConstSM (Just 2)) 3)

prop_suchThatT_simpleConst_GenOfNothing :: Property
prop_suchThatT_simpleConst_GenOfNothing = 
    forAll 
        (let (NextG inside) = constOfG True `suchThatT` isConstSM (Just False)
        in inside)
        isNothing 

prop_suchThatT_arbitraryAndContradiction_GenOfNothing :: Property
prop_suchThatT_arbitraryAndContradiction_GenOfNothing = 
    forAll 
        (let (NextG inside) = arbitraryStamage `suchThatT` (contradictionSM:: Stamate Int)
        in inside)
        isNothing 

prop_suchThatT_sameConst2_2strsOnly :: Property
prop_suchThatT_sameConst2_2strsOnly = 
    forAll 
        (stamageRun $ constOfG True `suchThatT` isConstSM (Just True))
        $ isConstVal True

prop_suchThatT_oddEvenTautolgy_IsOddEven :: Property
prop_suchThatT_oddEvenTautolgy_IsOddEven = 
    forAll 
        (stamageRun $ oddEven `suchThatT` tautologySM)
        (\aStr -> collect aStr $ alternatesOddEven aStr)

prop_suchThatT_oddEvenKeepOnlyPositive_IsOddEven :: Property
prop_suchThatT_oddEvenKeepOnlyPositive_IsOddEven = 
    forAll 
        (stamageRun $ oddEven `suchThatT` isPositive)
        (\aStr -> collect aStr $ alternatesOddEven aStr)



prop_mkStamage_trueConst_isTrueConst :: Property
prop_mkStamage_trueConst_isTrueConst = 
    forAll 
        (stamageRun $ mkStamage $ isConstSM (Just True))
        $ isConstVal True

return []
runTests :: IO Bool
runTests = $quickCheckAll




