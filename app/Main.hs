module Main (main) where
import PlayArea as PA
import StreamGen as SG
import Test.QuickCheck
import Rattus.Stream

runSG = do
    print "inc ints"
    sample (increasingNums:: Gen (Str Int))
    sample (increasingNums:: Gen (Str Float))
    sample (uniqueStr::Gen (Str String))
    sample (uniqueStr::Gen (Str Int))
    sample (constStrSM:: Gen (Str Int))
    sample (constStrSM:: Gen (Str (Bool, Float)))
    
main :: IO ()
main = do
    runSG
