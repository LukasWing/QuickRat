module Main (main) where
import PlayArea as PA
import Generators as G
import Test.QuickCheck
import Rattus.Stream hiding (map)
import LTL
import Helpers (constStr)
import Generators


runSG = do
    print "inc ints"
    
runLTL = do 
    print "LTL: ------"
    print "LTL: Done"


main :: IO ()
main = do
    runSG
    runLTL
    PA.run

