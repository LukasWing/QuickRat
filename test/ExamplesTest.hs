{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module ExamplesTest (
    runTests
) where
import Test.QuickCheck
import Examples
import Rattus.Stream
import Rattus
import LTL



return []
runTests :: IO Bool
runTests = $quickCheckAll




