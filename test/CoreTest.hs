{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module CoreTest (
    runTests
) where
import Core
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

--- Acceptor testing --------------------------------------------------------------
p :: Acceptor Int
p = NextA $ check odd
q :: Acceptor Int
q = NextA $ check even

pAndXQ = NextA $ \x1 -> if odd x1 then q else Reject

prop_accept_oddEven_only123accepted =
    accept pAndXQ (strExtend [1,2,3])

prop_accept_oddEven_223and138notAccepted =
    not (accept pAndXQ (strExtend [2,2,3]))
    && not (accept pAndXQ (strExtend [1,3,8]))


prop_mkAcceptor_x1EvenX2Odd_holdOnStreams :: Bool
prop_mkAcceptor_x1EvenX2Odd_holdOnStreams =
     accept
        (mkAcceptor $ Atom even `And` Imminently (Atom odd))
        $ strExtend [2,1]


prop_mkStamate_afterNAlwaysOdd :: Positive Int -> Bool
prop_mkStamate_afterNAlwaysOdd (Positive {getPositive=pN}) =
    let n = (pN `mod` 10) in
    accept
        (mkAcceptor (
                After n  (Always (Atom odd))
                `And`
                Not (After (n-1) (Atom odd))
        ))
        $ strExtend $ replicate n (2::Int) ++ [1]

prop_mkStamate_eventually :: Positive Int -> Bool
prop_mkStamate_eventually (Positive {getPositive=pN}) =
    let n = (pN `mod` 10) in
    accept
        (mkAcceptor (Eventually (Atom (==1))))
        (strExtend $ replicate n (2::Int) ++ [1,-1])

--- Next state functions testing -------------------------------------------------
prop_accept_anyStream_constsOK :: Str Bool -> NonNegative Int -> Bool
prop_accept_anyStream_constsOK aStr (NonNegative {getNonNegative=i}) =
    all (\acceptance -> evalAcceptor (acceptance aStr)) [
        accept' 0 $ NextA (const Reject),
        accept' i $ NextA (const Accept),
        accept' 0 Accept
    ]
    && not (evalAcceptor (accept' 0 Reject aStr))

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
    where f (h:::t) = (h>=(10::Int)) ::: delay (f (adv t))


f :: Str Int -> Str Bool
f _ = constStr False

prop_f_pBelow10_vAlwaysOff :: Property
prop_f_pBelow10_vAlwaysOff =
    forAll
        (trans $ mkTransducer $ Always (Atom (<10)))
        $ accept (mkAcceptor (Always (Atom not))) . f

prop_f_pBelow10_vAlwaysOff' :: Property
prop_f_pBelow10_vAlwaysOff' = 
    ltlProperty f (Always (Atom (<10))) (Always (Atom not))

return []
runTests :: IO Bool
runTests = $quickCheckAll