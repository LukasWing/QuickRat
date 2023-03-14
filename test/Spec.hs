import StreamGenTest as SG
import System.Exit 
import Test.QuickCheck
main :: IO ()
main = do
    good <- and <$> sequence [SG.runTests]
    if good
        then exitSuccess
        else exitFailure
