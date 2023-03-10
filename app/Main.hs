module Main (main) where

import PlayArea (run)

import StrUtils 
import ArbitraryStr as A


main :: IO ()
main = do
    run
    A.mainStr
