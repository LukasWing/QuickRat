{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module GeneratorTest (
    runTests
) where
import Generators
import Evaluators
import Helpers
import Test.QuickCheck
import Rattus.Stream
import Rattus
import LTL
import Types
import Functions


hasHead :: (Eq p) => p -> TPred p
hasHead expectedHead = SP (==expectedHead)

satisfies :: Show a => Stamage a -> TPred a -> Property
aStamage `satisfies` anLTL =
    forAll
        (stamageRun aStamage)
        $ evalLTL anLTL

prop_alternatesOddEven :: Property
prop_alternatesOddEven =
    forAll oddEvenGen alternatesOddEven

prop_constOfG_3_allThrees :: Property
prop_constOfG_3_allThrees =
    constOfG 3 `satisfies` Always (hasHead (3::Int))



evenOddGenPair :: [Gen Int]
evenOddGenPair = [
        evenGen,
        oddGen
    ]


nonChatty :: Args
nonChatty = stdArgs {chatty = True}

displayOnlyFailing :: Property -> IO Result
displayOnlyFailing aProperty = do
    result <- quickCheckWithResult nonChatty aProperty
    case result of
        Success {} -> return result
        _ ->  quickCheckResult aProperty

return []
runTests :: IO Bool
runTests = $forAllProperties displayOnlyFailing