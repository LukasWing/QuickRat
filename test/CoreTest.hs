{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module CoreTest (
    runTests
) where
import CoreRn (restrictWith, Transducer (..), arbitraryTransducer, mkAcceptor, Acceptor (..), TPred (..), accept, andA, strExtend)
import Test.QuickCheck (quickCheckAll, Property, forAll, Gen, Arbitrary (arbitrary), Positive (..))
import Data.Maybe ( isNothing )
import Data.Bits ((.|.))
import Control.Monad (liftM2)

oddGen :: Gen Int
oddGen = (.|.) 1 <$> arbitrary

evenGen :: Gen Int
evenGen = (2*) <$> arbitrary

prop_restictWith_arbitraryAndContradiction_GenOfNothing :: Property
prop_restictWith_arbitraryAndContradiction_GenOfNothing =
    forAll
        (let (NextT inside) = 
                (arbitraryTransducer:: Transducer Int) `restrictWith` mkAcceptor Contradiction 
        in inside)
        isNothing

prop_mkAcceptor_x1EvenX2Odd_holdOnStreams :: Bool
prop_mkAcceptor_x1EvenX2Odd_holdOnStreams =
     accept
        (strExtend [2,1])
        (mkAcceptor $ Atom even `And` Imminently (Atom odd))

x1Even :: Acceptor Int
x1Even = NextA (\x -> if even x then Accept else Reject)

x2Odd :: Acceptor Int
x2Odd = NextA (\_ -> NextA (\x1 -> if odd x1 then Accept else Reject))


prop_andA_x2Oddx1Even_23Accepts :: Property
prop_andA_x2Oddx1Even_23Accepts =
    forAll
        (liftM2 (,) oddGen evenGen)
        $ \(oddN, evenN) ->
            let NextA firstCheck = x2Odd `andA` x1Even
                NextA secondCheck = firstCheck evenN
            in  show (firstCheck evenN) == "N"
                && show (secondCheck oddN) == "A"

prop_andA_x2Oddx1Even_oddRejects :: Property
prop_andA_x2Oddx1Even_oddRejects =
    forAll
        oddGen
        $ \oddN ->
            let NextA firstCheck = x2Odd `andA` x1Even
            in  show (firstCheck oddN) == "R"

prop_mkStamate_afterNAlwaysOdd :: Positive Int -> Bool
prop_mkStamate_afterNAlwaysOdd (Positive {getPositive=pN}) =
    let n = (pN `mod` 10) in
    accept
        (strExtend $ replicate n (2::Int) ++ [1])
        $ mkAcceptor (After n  (Always (Atom odd))
                    `And`
                    Not (After (n-1) (Atom odd)))

return []
runTests :: IO Bool
runTests = $quickCheckAll