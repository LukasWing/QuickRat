{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where
import PlayArea as PA
import Test.QuickCheck
import System.CPUTime

myTime f = do 
    t0 <- getCPUTime
    print $ fromInteger t0 / 1000000
    f
    t1 <- getCPUTime
    print $ fromInteger t1 / 1000000

main :: IO ()
main = do
    myTime PA.run

