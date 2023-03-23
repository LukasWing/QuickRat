{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module LTLTest (runTests) where
import Generators hiding (next)
import Helpers
import Test.QuickCheck
import Rattus.Stream
import Rattus
import qualified Data.Set as Set
import Helpers
import Evaluators
import LTL

prop_SPConstTrue :: Bool
prop_SPConstTrue = 
    let expr5 = SP (\_ -> constStr True) in 
    evalLTL expr5 (constStr True) && evalLTL expr5 (constStr False)

prop_SPConstTrue_AnyIn :: Str String -> Bool
prop_SPConstTrue_AnyIn aStr = 
    let expr5 = SP (\_ -> constStr True) in 
    evalLTL expr5 aStr

return []
runTests :: IO Bool
runTests = $quickCheckAll
