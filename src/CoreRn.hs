{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CoreRn where
import Rattus.Stream (Str(..))
import Rattus.Primitives (delay, adv)
import Test.QuickCheck (Gen, arbitrary, Arbitrary, resize, forAll)
import Data.Maybe (fromJust)
import Test.QuickCheck.Property (Property)

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

instance Show a => Show (Transducer a) where
    show _ = "a Transducer"

instance (Show a) => Show (Str a) where
    show aStr =  "Str: " ++ (show . strTake (20::Int)) aStr ++ " ..."
        where 
        strTake n = strTake' n []
        strTake' picksLeft accumulator (h:::t) =
            if picksLeft > 0
                then strTake' (picksLeft - 1) (h:accumulator) (adv t)
                else reverse accumulator

--- Runners -----------------------------------------------------------------------
trans :: Transducer a -> Gen (Str a)
trans (NextT aGen) = do
    aMaybe <- aGen
    let (value, aTransducer) = fromJust aMaybe
    rest <- trans aTransducer
    return $ value ::: delay rest

accept :: Str a -> Acceptor a -> Bool
accept aStr anAcceptor  = accept' aStr anAcceptor 20

accept' :: Str a -> Acceptor a -> Int ->  Bool
accept' _ Accept _ = True
accept' _ Reject _ = False
accept' (h ::: t) (NextA makeNext) checksLeft =
    checksLeft == 0
    || case makeNext h of
            Accept -> True
            Reject -> False
            continuation -> accept' (adv t) continuation (checksLeft - 1)


--- Makers --------------------------------------------------------------------------
rejectTransducer :: Transducer a
rejectTransducer = NextT (return Nothing)

arbitraryTransducer :: forall a. (Arbitrary a) => Transducer a
arbitraryTransducer = NextT $ do
    element <- (arbitrary :: Gen a)
    return (Just (element, arbitraryTransducer))

mkTransducer :: (Arbitrary a) => Acceptor a -> Transducer a
mkTransducer anAcceptor = arbitraryTransducer `restrictWith` anAcceptor

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
        Until phi psi   -> mkAcceptor $ psi `Or` Imminently (phi `Until` psi)
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
andA (NextA f1) (NextA f2) =
    NextA $ \x1 ->
        -- trace ("debug:"++ show (NextA f1)) $
        case f1 x1 of
            Accept -> NextA f2
            Reject -> Reject
            NextA f1Inner -> NextA f1Inner `andA` f2 x1
andA Reject _ = Reject
andA _ Reject = Reject
andA Accept st1 = st1
andA st2 Accept = st2

restrictWith :: Transducer a -> Acceptor a -> Transducer a
restrictWith _ Reject = rejectTransducer
restrictWith aTransducer Accept = aTransducer
restrictWith (NextT gen) (NextA passTest) =
    let nTries = 1000
        sizeSuggest = 10
        loop n = do
            value <- resize sizeSuggest gen
            case value of
                Nothing -> return Nothing
                Just (genVal, nextGen) ->
                    -- trace ("genVal: "++ show genVal) $ 
                    case passTest genVal of
                        Accept -> return $ Just (genVal, nextGen)
                        Reject -> if n == 0 then return Nothing else loop (n-1)
                        anAcceptor -> return $ Just (genVal, restrictWith nextGen anAcceptor)
    in NextT $ loop (nTries::Int)


--- Utilities -----------------------------------------------------------------------------
ltlProperty :: (Arbitrary a, Show a) => (Str a -> Str b) -> TPred a -> TPred b -> Property
ltlProperty fUnderTest inPred outPred = 
    forAll 
        (trans $ (mkTransducer . mkAcceptor) inPred)
        $ \aStr -> accept (fUnderTest aStr) (mkAcceptor outPred)

    
f :: Str Int -> Str Bool
f aStr = error "NI"

constStr :: a -> Str a
constStr v = v ::: delay (constStr v)

strExtend :: [a] -> Str a
strExtend [h] = constStr h
strExtend (h:t) = h ::: delay (strExtend t) 
strExtend [] = error "No value in list"

prop_f_pBelow10_vAlwaysOff :: Property
prop_f_pBelow10_vAlwaysOff =
    ltlProperty f (Always (Atom (<10))) (Always (Atom not))



