{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
import GeneratorTest as GT
import HelpersTest as HT
import EvaluatorTest as ET
import CoreTest as CT
import System.Exit 
import Test.QuickCheck
import Test.QuickCheck.Test 
import Control.Monad.State
import qualified LTLTest as LT


main :: IO ()
main = do
    let testRunners =   [return True
                        -- , GT.runTests
                        -- , HT.runTests
                        -- , ET.runTests
                        -- , LT.runTests
                        , CT.runTests
                        ]
    good <- and <$> sequence testRunners
    if good
        then exitSuccess
        else exitFailure
