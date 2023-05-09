{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where
import PlayArea as PA
import Test.QuickCheck

main :: IO ()
main = do
    PA.run

