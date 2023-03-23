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
prop_NotIsInverse _aStr =
    evalLTL (Not (Not tautology)) _aStr

-- ~(~phi ^ phi) == true.
prop_AndContradictionAndTautologyContradiction :: Str Bool-> Bool
prop_AndContradictionAndTautologyContradiction anyStr =
    evalLTL (Not 
                (And 
                    (Not (mkSP anyStr)) 
                    (mkSP anyStr)
                )
            ) 
            anyStr

-- ~phi || phi == true.
prop_OrContradictionAndTautologyContradiction :: Str Bool -> Bool
prop_OrContradictionAndTautologyContradiction anyStr =
    evalLTL (Or 
                (Not (mkSP anyStr)) 
                (mkSP anyStr)
            ) 
            anyStr 

-- phi || T
prop_OrTIsNeutral :: Str Bool -> Bool
prop_OrTIsNeutral anyStr = 
    evalLTL (Or 
                (mkSP anyStr)
                tautology
            ) 
            anyStr

return []
runTests :: IO Bool
runTests = $quickCheckAll
