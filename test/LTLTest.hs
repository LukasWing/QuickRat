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
prop_AndContradictionAndTautologyContradiction :: Str Bool -> Str String -> Bool
prop_AndContradictionAndTautologyContradiction boolStr1  =
    evalLTL $ Not $ Not (mkSP boolStr1) `And` mkSP boolStr1

-- ~phi || phi.
prop_OrContradictionAndTautologyContradiction :: Str Bool -> Str String-> Bool
prop_OrContradictionAndTautologyContradiction boolStr1 =
    evalLTL $ Not (mkSP boolStr1) `Or` mkSP boolStr1

-- phi || T
prop_OrTIsNeutral :: Str Bool -> Str String -> Bool
prop_OrTIsNeutral boolStr1  =
    evalLTL $ mkSP boolStr1 `Or` tautology

-- ~ (F -> T)
prop_ImpliesGetsFalse :: Str String -> Bool
prop_ImpliesGetsFalse =
    evalLTL $ Not  (Not tautology) `Implies` tautology

-- prop_Always :: Str String -> Bool
-- prop_Always =
--     evalLTL $ Always tautology

prop_Eventually :: Property
prop_Eventually =
    forAll increasingNums $
        let positive = (>=(0::Int)) in
        evalLTL $ Eventually $ SP (RS.map (box positive))

phiBox :: TPred a
phiBox = mkSP $ fixedCyclicStr [False, False, False, False, False, True, True, True, True, True]

psiBox :: TPred a
psiBox = Not phiBox


prop_ImminentlyMoves1 :: Bool
prop_ImminentlyMoves1 = not phi4 && imminentPhi4
    where phi4 = False
          imminentPhi4 =  strGet 4 nextPhi
          nextPhi = evalLTL' (Imminently phiBox) (constStr Nothing)

prop_ImminentlyMovesAPeriod :: Bool
prop_ImminentlyMovesAPeriod = expected =~= actual
    where   expected = evalLTL'
                    (iterate Imminently phiBox !! 10)
                    (constStr Nothing)
                    
            actual = evalLTL' phiBox (constStr Nothing)

return []
runTests :: IO Bool
runTests = $quickCheckAll
