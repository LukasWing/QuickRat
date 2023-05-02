{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module LTLTest (runTests) where
import Generators hiding (next)
import Helpers
import Test.QuickCheck
import Rattus.Stream hiding (const, zip)
import qualified Rattus.Stream as RS hiding (const)
import Rattus
import qualified Data.Set as Set
import Evaluators
import LTL
import Functions

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

idSP = SP $ \i -> (i::Bool)


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
    evalLTL (Always idSP) (constStr True)

prop_UntilAllTrue :: Bool
prop_UntilAllTrue =
    evalLTL (Until idSP idSP) (constStr True)

prop_UntilPsiTrueAllTrue :: Bool
prop_UntilPsiTrueAllTrue =
    evalLTL (Until idSP $ Not idSP) (constStr True)

prop_UntilPhiTrueAllTrue :: Bool
prop_UntilPhiTrueAllTrue =
    evalLTL (Until (Not idSP) idSP) (constStr True)

prop_UntilBothFalseGivesFalse :: Bool
prop_UntilBothFalseGivesFalse =
    not $ evalLTL (Until idSP idSP) (constStr False)

prop_UntilPhiTurnsOffThenPhi :: Property
prop_UntilPhiTurnsOffThenPhi =
    forAll (arbitrary::Gen Int) $ \i ->
    i >= 0 ==>
    let phiPart =  [j < i | j <- [0..i]]
        psiPart =  [j == i | j <- [0..i]]
        inStream = strExtend $ zip phiPart psiPart
    in evalLTL (SP fst `Until` SP snd) inStream


prop_EventuallyTrueGivesTrue :: Bool
prop_EventuallyTrueGivesTrue =
    evalLTL (Eventually idSP) (constStr True)

prop_EventuallyBoolStep :: Bool
prop_EventuallyBoolStep =
    evalLTL (Eventually idSP) $ strExtend boolStep

prop_EventuallyOneTrue :: Bool
prop_EventuallyOneTrue =
    evalLTL (Eventually idSP) $ strExtend [False, True, False]

prop_EventuallyTrueMightBeContradiction :: Bool
prop_EventuallyTrueMightBeContradiction =
    evalLTL (Eventually idSP) (constStr False)

prop_AfterAllTrue :: Int -> Bool
prop_AfterAllTrue timeGap = evalLTL (After timeGap idSP) (constStr True)

prop_AfterFirstFalse :: Bool
prop_AfterFirstFalse  = not $ evalLTL (After 0 idSP) $ strExtend [False, True]

boolStep :: [Bool]
boolStep = [False, False, False, False, False, True, True, True, True, True]

phiBox :: TPred Bool
phiBox = idSP

psiBox :: TPred Bool
psiBox = Not phiBox


return []
runTests :: IO Bool
runTests = $quickCheckAll

