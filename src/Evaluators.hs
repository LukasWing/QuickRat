{-# OPTIONS_GHC -Wno-unused-imports #-}
module Evaluators where
import Generators
import Helpers
import Test.QuickCheck
import Rattus.Stream hiding (const)
import Rattus
import qualified Data.Set as Set
import LTL
import Control.Monad
import Debug.Trace (trace)
import GHC.RTS.Flags (ParFlags(setAffinity))
import TypeFromMeeting (mkStamage, mkStamate)
import Data.Bits (Bits(xor))

-- Foundations ----------------------------------------------------------------

data Stamate a = Pass
                | Fail
                | NextT (a -> Stamate a)

instance Show (Stamate a) where
    show Pass = "P"
    show Fail = "F"
    show (NextT _) = "N"

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
-- State Machines -------------------------------------------------------------

tautologySM' :: Stamate a
tautologySM' = Pass

tautologySM :: Stamate a
tautologySM = NextT (const Pass)

contradictionSM :: Stamate a
contradictionSM = NextT (const Fail)

isConstSM :: Eq a => Maybe a -> Stamate a
isConstSM input =
  case input of
    Just value ->
      NextT
        ( \inp ->
            if inp == value
              then isConstSM (Just inp)
              else Fail
        )
    Nothing -> NextT (isConstSM . Just)

isHeadEqualSM ::(Eq a) => Str a -> Stamate a
isHeadEqualSM (h ::: _) =
    NextT (\value ->
            if h == value
                then Pass
                else Fail
            )


isPositive :: Stamate Int
isPositive = NextT (\n -> if n > 0 then isPositive else Fail)


-- Testables ------------------------------------------------------------------

isConstCheck :: (Eq a) => Str a -> Bool
isConstCheck aStr = stamateRun aStr (isConstSM Nothing)

isConstVal :: (Eq a) => a -> Str a -> Bool
isConstVal val aStr = stamateRun aStr (isConstSM (Just val))

areIncreasing :: (Ord a) => Str a -> Bool
areIncreasing (h1 ::: t1) =
    let isIncreasing e1 (e2 ::: t2) checksLeft =
            checksLeft == 0
            || (e1 <= e2)
            && isIncreasing e2 (adv t2) (checksLeft - 1)
        checks = (20::Int)
    in isIncreasing h1 (adv t1) checks

allTrue :: Str Bool -> Bool
allTrue aStr = stamateRun aStr $ isConstSM (Just True)

allFalse :: Str Bool -> Bool
allFalse aStr = stamateRun aStr $ isConstSM (Just False)

strProbEq :: Eq a => Str a -> Str a -> Bool
strProbEq s1 s2 = stamateRun s1 $ strProbEq' s2
    where
        strProbEq' (h ::: t) = NextT (
            \value ->
                if value == h
                    then strProbEq' (adv t)
                    else Pass
            )

--- Stamage modifiers --------------------------------------------------------
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


-- Move 
mkStamage :: (Arbitrary a) => Stamate a -> Stamage a
mkStamage aStamate = arbitraryStamage `suchThatT` aStamate



mkStamate' :: TPred a -> Stamate a
mkStamate' formulae =
    case formulae of
            SP headPred     -> NextT (\h -> if headPred h then Pass else Fail)
            Not aTPred      -> errorNotImplemented
            Or phi psi      -> errorNotImplemented
            And phi psi     -> mkStamate' phi `andT'` mkStamate' psi                                           
            Implies phi psi -> errorNotImplemented
            Imminently phi  -> mkStamate' phi
            Eventually phi  -> case mkStamate' phi of
                                Pass -> Pass
                                Fail -> mkStamate' $ Eventually phi
                                NextT nextF -> NextT nextF
            Until phi psi   ->  mkStamate' phi 
            Always phi      -> errorNotImplemented
            After anInt phi -> if anInt == 0
                                then mkStamate' phi
                                else mkStamate' (After (anInt - 1) phi)
            _ -> errorNotImplemented

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



-- sAnd st1 st2 = NextT $ f st1 st2
--   where
--     f Fail _ _ = Fail
--     f _ Fail _ = Fail
--     f Pass Pass _ = Pass
--     f Pass (NextT t1) x = sAnd Pass (t1 x) 
--     f (NextT t1) Pass x = sAnd Pass (t1 x)
--     f (NextT t1) (NextT t2) x = sAnd (t1 x) (t2 x) 

-- sAnd :: Stamate a -> Stamate a -> Stamate a
-- sAnd Pass Pass = Pass
-- sAnd _ Fail = Fail
-- sAnd Fail _ = Fail
-- sAnd (NextT t1) Pass = NextT $ \x -> sAnd (t1 x) Pass
-- sAnd Pass (NextT t2) = NextT $ \x -> sAnd Pass (t2 x)
-- sAnd (NextT t1) (NextT t2) = NextT $ \x -> sAnd (t1 x) (t2 x)

    -- where 
        -- f:: Stamate a -> Stamate a -> a -> Stamate a
        -- f = 
        -- f Pass (NextT testerF)  x = testerF x
        -- f (NextT testerF) Pass  x = testerF x
        -- f (NextT testerF1) (NextT testerF2) = f (testerF1 x) (testerF2 x) 