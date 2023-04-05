{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
import GeneratorTest as GT
import HelpersTest as HT
import EvaluatorTest as ET
import System.Exit 
import Test.QuickCheck
import Test.QuickCheck.Test 
import Control.Monad.State
import qualified LTLTest as LT


main :: IO ()
main = do
    let testRunners =   [
                        GT.runTests
                        -- , HT.runTests
                        , ET.runTests
                        --, LT.runTests
                        ]
    good <- and <$> sequence testRunners
    if good
        then exitSuccess
        else exitFailure
