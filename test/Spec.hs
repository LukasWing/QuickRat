{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
import CoreTest as CT
import System.Exit 
import Test.QuickCheck
import Test.QuickCheck.Test 
import Control.Monad.State

main :: IO ()
main = do
    let testRunners = [CT.runTests]
    good <- and <$> sequence testRunners
    if good
        then exitSuccess
        else exitFailure
