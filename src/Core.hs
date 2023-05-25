{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Core where
import Rattus.Stream (Str(..))
import Rattus.Primitives (delay, adv)
import Test.QuickCheck
    ( Gen, arbitrary, Arbitrary(..), forAll, scale, getSize )
import Data.Maybe (fromJust)
import Test.QuickCheck.Property (Property)
import Debug.Trace (trace)

--- Types ----------------------------------------------------------------------------
data TPred a where
    Tautology       :: TPred a
    Contradiction   :: TPred a
    Atom            :: (a -> Bool) -> TPred a
    Not             :: TPred a -> TPred a
    Or              :: TPred a -> TPred a -> TPred a
    Until           :: TPred a -> TPred a -> TPred a
    Imminently      :: TPred a -> TPred a
    And             :: TPred a -> TPred a -> TPred a
    Implies         :: TPred a -> TPred a -> TPred a
    Always          :: TPred a -> TPred a
    Eventually      :: TPred a -> TPred a
    After           :: Int -> TPred a -> TPred a

data Acceptor a = Accept
                | Reject
                | NextA (a -> Acceptor a)

instance Show (Acceptor a) where
    show Accept     = "A"
    show Reject     = "R"
    show (NextA _)  = "N"

newtype Transducer a = NextT (Gen (Maybe (a, Transducer a)))

instance (Arbitrary a) => Arbitrary (Transducer a) where
    arbitrary = return arbitraryTransducer

instance Show a => Show (Transducer a) where
    show _ = "a Transducer"

strTake :: Int -> Str a -> [a]
strTake n = strTake' n [] where 
    strTake' picksLeft accumulator (h:::t) =
        if picksLeft > 0
            then strTake' (picksLeft - 1) (h:accumulator) (adv t)
            else reverse accumulator

instance (Show a) => Show (Str a) where
    show aStr =  "Str: " ++ (show . strTake 20) aStr ++ " ..."

class ProbEq a where
    (=~=) :: a -> a -> Bool

instance (Eq a) => ProbEq (Str a) where
    stream1 =~= stream2 = strTake 20 stream1 == strTake 20 stream2

instance (Arbitrary a)  => Arbitrary (Str a) where
    arbitrary = trans arbitraryTransducer

--- Runners -----------------------------------------------------------------------
trans :: Transducer a -> Gen (Str a)
trans (NextT aGen) = do
    aMaybe <- aGen
    let (value, aTransducer) = fromJust aMaybe
    rest <- trans aTransducer
    return $ value ::: delay rest

accept ::  Acceptor a -> Str a -> Bool
accept anAcceptor  = evalAcceptor . accept' 20 anAcceptor

accept' :: Int -> Acceptor a -> Str a ->  Acceptor a
accept' checksLeft  (NextA makeNext) (h ::: t) =
    if checksLeft == 0
        then NextA makeNext
        else case makeNext h of
            Accept -> Accept
            Reject -> Reject
            continuation -> accept' (checksLeft - 1) continuation (adv t)
accept'  _ rejectOrAccept _ = rejectOrAccept

evalAcceptor :: Acceptor a -> Bool
evalAcceptor Accept = True
evalAcceptor Reject = False
evalAcceptor (NextA _) = True

check :: (a -> Bool) -> (a -> Acceptor a)
check predicate x = if predicate x then Accept else Reject


--- Makers --------------------------------------------------------------------------
rejectTransducer :: Transducer a
rejectTransducer = NextT (return Nothing)

arbitraryTransducer :: forall a. (Arbitrary a) => Transducer a
arbitraryTransducer = NextT $ do
    element <- (arbitrary :: Gen a)
    return (Just (element, arbitraryTransducer))

guidedTransducer :: forall a. (Arbitrary a) => Gen a -> Transducer a
guidedTransducer aGen = NextT $ do
    element <- aGen 
    return (Just (element, guidedTransducer aGen))

ofAcceptor :: (Arbitrary a, Show a) => Acceptor a -> Transducer a
ofAcceptor anAcceptor = arbitraryTransducer `restrictWith` anAcceptor

mkTransducer :: (Arbitrary a, Show a) => TPred a -> Transducer a
mkTransducer = ofAcceptor . mkAcceptor

mkAcceptor :: TPred a -> Acceptor a
mkAcceptor formulae =
    case formulae of
        Tautology       -> Accept
        Contradiction   -> Reject
        Atom headPred   -> NextA (\h -> if headPred h then Accept else Reject)
        Not phi         -> negateA $ mkAcceptor phi
        Or phi psi      -> mkAcceptor (Not (Not phi `And` Not psi))
        And phi psi     -> mkAcceptor phi `andA` mkAcceptor psi
        Implies phi psi -> mkAcceptor (Not phi `Or` psi)
        Imminently phi  -> NextA (\_ -> mkAcceptor phi)
        Eventually phi  -> mkAcceptor $ phi `Or` Imminently (Eventually phi)
        Until phi psi   -> mkAcceptor $ psi `Or` (phi `And` Imminently (phi `Until` psi))
        Always phi      -> mkAcceptor $ phi `And` Imminently (Always phi)
        After anInt phi -> mkAcceptor $ if anInt == 0
                                        then phi
                                        else Imminently (After (anInt - 1) phi)

--- Modifiers ------------------------------------------------------------------------

negateA :: Acceptor a -> Acceptor a
negateA Reject = Accept
negateA Accept = Reject
negateA (NextA f) =  NextA (negateA . f)

andA :: Acceptor a -> Acceptor a -> Acceptor a
andA (NextA f1) (NextA f2) = NextA $ \x1 -> f1 x1 `andA` f2 x1
andA Reject _ = Reject
andA _ Reject = Reject
andA Accept st = st
andA st Accept = st

restrictWith :: forall a.  Show a => Transducer a -> Acceptor a -> Transducer a
restrictWith _ Reject = rejectTransducer
restrictWith aTransducer Accept = aTransducer
restrictWith aTransducer (NextA someTest) = rwInner aTransducer someTest

rwInner :: forall a. Show a => Transducer a -> (a -> Acceptor a) -> Transducer a 
rwInner (NextT gen) passTest = NextT $ loop (1000::Int) where 
    loop :: Int -> Gen (Maybe (a, Transducer a))
    loop n = do
        sz <- getSize
        value <- if  sz < 3 then scale (+3) gen else gen
        case value of
            Nothing -> return Nothing
            Just (genVal, xGen) -> 
                -- trace ("genVal: "++ show genVal) $
                case passTest genVal of
                Accept -> return $ Just (genVal, xGen)
                Reject -> if n == 0 then return Nothing else loop (n - 1)
                NextA passTest' -> return $ Just (genVal, rwInner xGen passTest')

--- Utilities -----------------------------------------------------------------------------
ltlProperty :: (Arbitrary a, Show a) => (Str a -> Str b) -> TPred a -> TPred b -> Property
ltlProperty fUnderTest inPred outPred =
    forAll
        (trans $ mkTransducer inPred)
        $ accept (mkAcceptor outPred)  . fUnderTest


constTransducer :: a -> Transducer a
constTransducer value = NextT $ return (Just (value, constTransducer value))

transducerOfStr :: Str a -> Transducer a
transducerOfStr (h ::: t) = NextT $ return $ Just (h, transducerOfStr (adv t))

constStr :: a -> Str a
constStr v = v ::: delay (constStr v)

strExtend :: [a] -> Str a
strExtend [h] = constStr h
strExtend (h:t) = h ::: delay (strExtend t)
strExtend [] = error "No value in list"


evalLTL :: TPred a -> Str a -> Bool
evalLTL = evalLTL' 20

evalLTL' :: Int -> TPred a -> Str a -> Bool
evalLTL' checksLeft formulae aStr@(h ::: t) = checksLeft <= 0 
    || case formulae of
        Tautology       -> True
        Contradiction   -> False
        Atom headPred   -> headPred h
        Not aTPred      -> not $ eval aTPred aStr
        Or phi psi      -> eval phi aStr || eval psi aStr
        And phi psi     -> eval phi aStr && eval psi aStr
        Implies phi psi -> eval (Not phi `Or` psi) aStr
        Imminently phi  -> evalNext phi strTail
        Eventually phi  -> eval phi aStr || evalNext (Eventually phi) strTail
        Until phi psi   -> eval psi aStr || (eval phi aStr && evalNext (phi `Until` psi) strTail)
        Always phi      -> eval phi aStr && evalNext (Always phi) strTail
        After anInt phi -> if anInt == 0
                            then eval phi aStr
                            else evalNext (After (anInt - 1) phi) strTail
    where   
        evalNext = evalLTL' (checksLeft - 1)
        eval = evalLTL' checksLeft
        strTail = adv t
