{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module CoreTest (
    runTests
) where
import CoreRn 
import Test.QuickCheck (quickCheckAll, Property, forAll, Gen, Arbitrary (arbitrary), Positive (..), NonNegative (..))
import Data.Maybe (isNothing )
import Data.Bits ((.|.))
import Control.Monad (liftM2)
import Rattus.Primitives (delay, adv)
import Rattus.Stream (Str(..))

--- Acceptor modifiers ------------------------------------------------------------------------
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

--- Acceptor testing --------------------------------------------------------------
prop_mkAcceptor_x1EvenX2Odd_holdOnStreams :: Bool
prop_mkAcceptor_x1EvenX2Odd_holdOnStreams =
     accept
        (strExtend [2,1])
        (mkAcceptor $ Atom even `And` Imminently (Atom odd))

x1Even :: Acceptor Int
x1Even = NextA (\x -> if even x then Accept else Reject)

x2Odd :: Acceptor Int
x2Odd = NextA (\_ -> NextA (\x1 -> if odd x1 then Accept else Reject))

prop_mkStamate_afterNAlwaysOdd :: Positive Int -> Bool
prop_mkStamate_afterNAlwaysOdd (Positive {getPositive=pN}) =
    let n = (pN `mod` 10) in
    accept
        (strExtend $ replicate n (2::Int) ++ [1])
        $ mkAcceptor (After n  (Always (Atom odd))
                    `And`
                    Not (After (n-1) (Atom odd)))

prop_mkStamate_eventually :: Positive Int -> Bool
prop_mkStamate_eventually (Positive {getPositive=pN}) =
    let n = (pN `mod` 10) in
    accept
        (strExtend $ replicate n (2::Int) ++ [1,-1])
        $ mkAcceptor (Eventually (Atom (==1)))

--- Next state functions testing -------------------------------------------------
prop_accept_anyStream_constsOK :: Str Bool -> NonNegative Int -> Bool
prop_accept_anyStream_constsOK aStr (NonNegative {getNonNegative=i}) =
    not $ evalAcceptor (accept' aStr Reject 0)
    && evalAcceptor (accept' aStr (NextA (const Reject)) 0)
    && evalAcceptor (accept' aStr Accept 0)
    && evalAcceptor (accept' aStr (NextA (const Accept)) i)

satisfies :: Show a => Transducer a -> TPred a -> Property
aTransducer `satisfies` aTPred =
    forAll
        (trans aTransducer)
        $ evalLTL aTPred

prop_trans_constOfThree_allThrees :: Property 
prop_trans_constOfThree_allThrees =
    constTransducer (3::Int) `satisfies` Always (Atom (==3))
    
prop_ltlProperty_pred_fPasses :: Property
prop_ltlProperty_pred_fPasses =
    ltlProperty f (Always (Atom (<10))) (Always (Atom not))
    where f (h:::t) = (h>=(10::Int)) ::: delay ( f(adv t))

return []
runTests :: IO Bool
runTests = $quickCheckAll