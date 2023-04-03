{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module LTLTest (runTests) where
import Generators hiding (next)
import Helpers
import Test.QuickCheck
import Rattus.Stream hiding (const)
import qualified Rattus.Stream as RS hiding (const)
import Rattus
import qualified Data.Set as Set
import Helpers
import Evaluators
import LTL 


-- evalLTL tests ----------------------------------------------------------
prop_SPConstTrue :: Bool
prop_SPConstTrue =
    let expr5 = tautology in
    evalLTL expr5 (constStr True) && evalLTL expr5 (constStr False)

prop_SPConstTrue_AnyIn :: Str String -> Bool
prop_SPConstTrue_AnyIn aStr =
    let expr5 = tautology in
    evalLTL expr5 aStr

prop_NotContractionTrue :: Str String -> Bool
prop_NotContractionTrue aStr =
    let nExpr = Not contradiction in
    evalLTL nExpr aStr

idSP = SP (strHead)

prop_NotIsInverse :: Str Bool -> Bool
prop_NotIsInverse = evalLTL $ Not $ Not tautology
 
-- ~(~phi ^ phi) == true.
prop_AndContradictionAndTautologyContradiction :: Str Bool -> Bool
prop_AndContradictionAndTautologyContradiction =
    evalLTL (Not $ Not idSP `And` idSP)


-- ~phi || phi.
prop_OrContradictionAndTautologyContradiction :: Str Bool-> Bool
prop_OrContradictionAndTautologyContradiction  =
    evalLTL $ Not idSP `Or` idSP

-- phi || T
prop_OrTIsNeutral :: Str Bool -> Bool
prop_OrTIsNeutral =
    evalLTL $ idSP `Or` tautology

-- ~ (F -> T)
prop_ImpliesGetsFalse :: Str String -> Bool
prop_ImpliesGetsFalse =
    evalLTL $ Not  (Not tautology) `Implies` tautology

prop_Always ::  Bool
prop_Always =
    checkLTL (Always idSP) (constStr True)

prop_UntilAllTrue :: Bool
prop_UntilAllTrue =
    checkLTL (Until idSP idSP) (constStr True)

prop_UntilPsiTrueAllTrue :: Bool
prop_UntilPsiTrueAllTrue =
    checkLTL (Until idSP $ Not (idSP)) (constStr True)

prop_UntilPhiTrueAllTrue :: Bool
prop_UntilPhiTrueAllTrue =
    checkLTL (Until (Not idSP) idSP) (constStr True)
    
prop_UntilBothFalseGivesFalse :: Bool
prop_UntilBothFalseGivesFalse =
    not $ checkLTL (Until idSP idSP) (constStr False)

prop_EventuallyTrueGivesTrue :: Bool
prop_EventuallyTrueGivesTrue =
    evalLTL (Eventually idSP) (constStr True)

prop_EventuallyBoolStep :: Bool
prop_EventuallyBoolStep =
    evalLTL (Eventually idSP) $ padFinite boolStep

prop_EventuallyOneTrue :: Bool
prop_EventuallyOneTrue =
    evalLTL (Eventually idSP) $ padFinite [False, True, False]

prop_EventuallyTrueMightBeContradiction :: Bool
prop_EventuallyTrueMightBeContradiction =
    evalLTL (Eventually idSP) (constStr False)

prop_AfterAllTrue :: Int -> Bool
prop_AfterAllTrue timeGap = evalLTL (After timeGap idSP) (constStr True)

prop_AfterFirstFalse :: Bool
prop_AfterFirstFalse  = not $ evalLTL (After 0 idSP) $ padFinite [False, True]


prop_Eventually :: Property
prop_Eventually =
    forAll increasingNums $
        let positive = (>=(0::Int)) in
        evalLTL $ Eventually $ SP (positive . strHead)



boolStep :: [Bool]
boolStep = [False, False, False, False, False, True, True, True, True, True]

phiBox :: TPred Bool
phiBox = idSP

psiBox :: TPred Bool
psiBox = Not phiBox

return []
runTests :: IO Bool
runTests = $quickCheckAll

