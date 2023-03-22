{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module HelpersTest (runTests) where
import Generators hiding (next)
import Helpers
import Test.QuickCheck
import Rattus.Stream
import Rattus
import qualified Data.Set as Set


prop_constStreamsAreEqual :: Int -> Bool
prop_constStreamsAreEqual v =
    constStr v =~= constStr v

threesGen :: Gen (Str Int, Str Int)
threesGen = scale (`mod` 2) (arbitrary :: Gen (Str Int, Str Int))

prop_headEqualIfAlmostEqual :: Property
prop_headEqualIfAlmostEqual =
    forAll threesGen $ \(as1, as2) ->
        as1 =~= as2 ==> strTake 2 as1 == strTake 2 as2

prop_headNotEqual_notAlmostEqual :: Str Int -> Str Int -> Property
prop_headNotEqual_notAlmostEqual as1 as2 =
    strTake 2 as1 /= strTake 2 as2 ==> not (as1 =~= as2)

return []
runTests :: IO Bool
runTests = $quickCheckAll