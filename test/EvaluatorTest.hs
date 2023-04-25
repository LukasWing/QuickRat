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

-- prop_suchThatT_constOfNegativeButNonNegativeAccepted_GenOfNothing :: Negative Int ->  NonNegative Int -> Bool
-- prop_suchThatT_constOfNegativeButNonNegativeAccepted_GenOfNothing 
--     (Negative {getNegative=i}) (NonNegative {getNonNegative=j}) = 
--         let actualG = constOfG i `suchThatT` isConstSM (Just j) 
--         in stamageRun actualG =~= constStr (Nothing:: Maybe Int)



-- prop_suchThatT_impleConst_GenOfNothing :: Negative Int ->  NonNegative Int -> Bool
-- prop_suchThatT_simpleConst_GenOfNothing :: Property
-- prop_suchThatT_simpleConst_GenOfNothing = 
--     forAll 
--         (let (NextG inside) = constOfG True `suchThatT` isConstSM (Just False)
--         in inside)
--         isNothing 

-- prop_suchThatT_arbitraryAndContradiction_GenOfNothing :: Property
-- prop_suchThatT_arbitraryAndContradiction_GenOfNothing = 
--     forAll 
--         (let (NextG inside) = arbitraryStamage `suchThatT` (contradictionSM:: Stamate Int)
--         in inside)
--         isNothing 

-- prop_suchThatT_sameConst2_2strsOnly :: Property
-- prop_suchThatT_sameConst2_2strsOnly = 
--     forAll 
--         (stamageRun $ constOfG True `suchThatT` isConstSM (Just True))
--         $ isConstVal True

-- prop_suchThatT_oddEvenTautolgy_IsEven :: Property
-- prop_suchThatT_oddEvenTautolgy_IsEven = 
--     forAll 
--         (stamageRun $ oddEven `suchThatT` tautologySM)
--         (\x -> collect x $ alternatesOddEven x)

prop_suchThatT_oddEvenKeepOnlyPositive_IsAlternating :: Property
prop_suchThatT_oddEvenKeepOnlyPositive_IsAlternating = 
    forAll 
        (stamageRun $ oddEven `suchThatT` isPositive)
        (\x -> collect x $ alternatesOddEven x)

return []
runTests :: IO Bool
runTests = $quickCheckAll




