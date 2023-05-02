{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Core where 
import Rattus.Stream (Str(..))
import Rattus.Primitives (delay, adv)
import Test.QuickCheck (Gen, arbitrary, Arbitrary, resize)

--- Types ----------------------------------------------------------------------------
data TPred a where
    SP          ::  (a -> Bool) -> TPred a
    Not         :: TPred a -> TPred a
    Or          :: TPred a -> TPred a -> TPred a
    Until       :: TPred a -> TPred a -> TPred a
    Imminently  :: TPred a -> TPred a
    And         :: TPred a -> TPred a -> TPred a
    Implies     :: TPred a -> TPred a -> TPred a
    Always      :: TPred a -> TPred a
    Eventually  :: TPred a -> TPred a
    After       :: Int -> TPred a -> TPred a

data Stamate a = Pass
            | Fail
            | NextT (a -> Stamate a)

instance Show (Stamate a) where
    show Pass = "P"
    show Fail = "F"
    show (NextT _) = "N"

newtype Stamage a = NextG (Gen (Maybe (a, Stamage a)))

instance Show a => Show (Stamage a) where
  show _ = "a Stamage" 

--- Runners -----------------------------------------------------------------------
stamageRun :: Stamage a -> Gen (Str a) 
stamageRun (NextG aGen) = do
    aMaybe <- aGen
    let (value, aStamage) = getJust aMaybe
    rest <- stamageRun aStamage
    return $ value ::: delay rest
    where   getJust (Just a) = a
            getJust Nothing = error "Generator ran out of values"

stamateRun :: Str a -> Stamate a -> Bool
stamateRun aStr aStamate  = stamateRun' aStr aStamate 20

stamateRun' :: Str a -> Stamate a -> Int ->  Bool
stamateRun' _ Pass _ = True
stamateRun' _ Fail _ = False
stamateRun' (h ::: t) (NextT makeNext) checksLeft =
    (checksLeft == 0) || (case makeNext h of
            Pass -> True
            Fail -> False
            continuation -> stamateRun' (adv t) continuation (checksLeft - 1))

--- Makers --------------------------------------------------------------------------
emptyStamage :: Stamage a
emptyStamage = NextG (return Nothing)

arbitraryStamage :: forall a. (Arbitrary a) => Stamage a
arbitraryStamage = NextG $ do
    element <- (arbitrary :: Gen a)
    return (Just (element, arbitraryStamage))

mkStamage :: (Arbitrary a) => Stamate a -> Stamage a
mkStamage aStamate = arbitraryStamage `suchThatT` aStamate

mkStamate :: TPred a -> Stamate a
mkStamate formulae =
    case formulae of
            SP headPred     -> NextT (\h -> if headPred h then Pass else Fail)
            Not phi         -> negateStamate $ mkStamate phi
            Or phi psi      -> mkStamate (Not (Not phi `And` Not psi)) 
            And phi psi     -> mkStamate phi `andT'` mkStamate psi
            Implies phi psi -> mkStamate (Not phi `Or` psi)
            Imminently phi  -> NextT (\_ -> mkStamate phi)
            Eventually phi  -> mkStamate $ phi `Or` Imminently (Eventually phi)
            Until phi psi   -> mkStamate $ psi `Or` Imminently (phi `Until` psi)
            Always phi      -> mkStamate $ phi `And` Imminently (Always phi)
            After anInt phi -> mkStamate $ if anInt == 0
                                            then phi
                                            else Imminently (After (anInt - 1) phi)

--- Modifiers ------------------------------------------------------------------------
negateStamate :: Stamate a -> Stamate a
negateStamate Fail = Pass
negateStamate Pass = Fail
negateStamate (NextT f) =  NextT (negateStamate . f)


andT' :: Stamate a -> Stamate a -> Stamate a
andT' (NextT f1) (NextT f2) =
    NextT $ \x1 ->
        -- trace ("debug:"++ show (NextT f1)) $
        case f1 x1 of
            Pass -> NextT f2
            Fail -> Fail
            NextT f1Inner -> NextT f1Inner `andT'` f2 x1

andT' Fail _ = Fail
andT' _ Fail = Fail
andT' Pass Pass = Pass
andT' Pass st1 = st1
andT' st2 Pass = st2

suchThatT :: Stamage a -> Stamate a -> Stamage a
suchThatT _ Fail = emptyStamage
suchThatT aStamage Pass = aStamage
suchThatT (NextG gen) (NextT passTest) =
    let nTries = 1000
        sizeSuggest = 10
        loop n = do
            value <- resize sizeSuggest gen
            case value of
                Nothing -> return Nothing
                Just (genVal, nextGen) ->
                    -- trace ("genVal: "++ show genVal) $ 
                    case passTest genVal of
                        Pass -> return $ Just (genVal, nextGen)
                        Fail -> if n == 0 then return Nothing else loop (n-1)
                        aStamate -> return $ Just (genVal, suchThatT nextGen aStamate)
    in NextG $ loop (nTries::Int)