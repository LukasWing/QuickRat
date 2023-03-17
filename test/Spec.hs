{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
import StreamGenTest as SG
import System.Exit 
import Test.QuickCheck
import PlayArea 
import Test.QuickCheck.Test 
import Control.Monad.State
import PlayArea (push')

prop_stackPushedIsPopped anInt =
    let (_, newStack) = push anInt [] in
    let (expectedAnInt, _) = pop newStack in
    anInt == expectedAnInt  
    
stackManip:: Int -> State Stack Int
stackManip anInt = do
    push' anInt
    pop'

prop_stackPushedIsPopped' anInt = 
    let (expectedInt, _) = runState (stackManip anInt) []
    in expectedInt == anInt

return []
runSpecTests = $quickCheckAll


main :: IO ()
main = do
    good <- and <$> sequence [SG.runTests]
    goodSpec <- runSpecTests
    if good && goodSpec
        then exitSuccess
        else exitFailure
