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
    let expr5 = SP (\_ -> constStr True) in
    evalLTL expr5 (constStr True) && evalLTL expr5 (constStr False)

prop_SPConstTrue_AnyIn :: Str String -> Bool
prop_SPConstTrue_AnyIn aStr =
    let expr5 = SP (\_ -> constStr True) in
    evalLTL expr5 aStr

prop_NotAllFalse :: Str String -> Bool
prop_NotAllFalse aStr =
    let nExpr = Not (SP (\_ -> constStr False)) in
    evalLTL nExpr aStr

prop_NotContractionTrue :: Bool
prop_NotContractionTrue =
    evalLTL (Not contradiction) aStr
    where
        contradiction = SP (RS.map (box (>10)))
        aStr = constStr (0::Int)

prop_NotIsInverse :: Str Bool -> Bool
prop_NotIsInverse = evalLTL $ Not $ Not tautology

-- ~(~phi ^ phi) == true.
prop_AndContradictionAndTautologyContradiction :: Str Bool -> Bool
prop_AndContradictionAndTautologyContradiction =
    evalLTL (Not $ Not idSP `And` idSP)

    -- evalLTL (Not $ Not idSP `And` idSP) boolStr1

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
    checkLTL (Until idSP (SP negateStr)) (constStr True)

prop_UntilPhiTrueAllTrue :: Bool
prop_UntilPhiTrueAllTrue =
    checkLTL (Until (SP negateStr) idSP) (constStr True)
    
prop_UntilBothFalseGivesFalse :: Bool
prop_UntilBothFalseGivesFalse =
    not $ checkLTL (Until idSP idSP) (constStr False)

prop_Eventually :: Property
prop_Eventually =
    forAll increasingNums $
        let positive = (>=(0::Int)) in
        evalLTL $ Eventually $ SP (RS.map (box positive))

boolStep :: [Bool]
boolStep = [False, False, False, False, False, True, True, True, True, True]

phiBox :: TPred Bool
phiBox = SP id

psiBox :: TPred Bool
psiBox = Not phiBox

prop_ImminentlyMoves1 :: Bool
prop_ImminentlyMoves1 = not phi4 && imminentPhi4
    where   phi4 = boolStep !! 4
            nextPhi = evalLTL' (Imminently phiBox) (fixedCyclicStr boolStep)
            imminentPhi4 = strGet 4 nextPhi

prop_ImminentlyMovesAPeriod :: Bool
prop_ImminentlyMovesAPeriod = expected =~= actual
    where   expected = evalLTL'
                        (iterate Imminently phiBox !! 10)
                        (fixedCyclicStr boolStep)
            actual = evalLTL' phiBox (fixedCyclicStr boolStep)


return []
runTests :: IO Bool
runTests = $quickCheckAll

