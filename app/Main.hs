module Main (main) where
import PlayArea as PA
import Generators as G
import Test.QuickCheck
import Rattus.Stream hiding (map)
import LTL
import Helpers (constStr)


runSG = do
    print "inc ints"
    sample (increasingNums:: Gen (Str Int))
    sample (increasingNums:: Gen (Str Float))
    sample (uniqueStr::Gen (Str String))
    sample (uniqueStr::Gen (Str Int))
    sample (constStrSM:: Gen (Str Int))
    sample (constStrSM:: Gen (Str (Bool, Float)))
    
runLTL = do 
    print "LTL: ------"
    let expr5 = SP (\_ -> constStr True)
    let expr3 = And expr5 expr5
    let expr4 = Not $ And expr5 expr5
    print "LTL: Done"


main :: IO ()
main = do
    runSG
    runLTL
    -- PA.run
