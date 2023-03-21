{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
import StreamGenTest as SG
import System.Exit 
import Test.QuickCheck
import Test.QuickCheck.Test 
import Control.Monad.State


main :: IO ()
main = do
    good <- and <$> sequence [SG.runTests]
    if good
        then exitSuccess
        else exitFailure
