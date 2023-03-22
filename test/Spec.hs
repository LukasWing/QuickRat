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


main :: IO ()
main = do
    good <- and <$> sequence [GT.runTests, HT.runTests, ET.runTests]
    if good
        then exitSuccess
        else exitFailure
