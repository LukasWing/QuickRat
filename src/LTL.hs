{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE GADTs #-}
module LTL where
import Rattus.Stream hiding (const)
import qualified Rattus.Stream as RS
import Evaluators
import Helpers
import Rattus

idSP :: TPred Bool
idSP = SP id

type StrPred a = Str a -> Str Bool --SM

data TPred a where
    SP          :: StrPred a -> TPred a
    Not         :: TPred a -> TPred a
    Or          :: TPred a -> TPred a -> TPred a
    Until       :: TPred a -> TPred a -> TPred a
    Imminently  :: TPred a -> TPred a
    And         :: TPred a -> TPred a -> TPred a
    Implies     :: TPred a -> TPred a -> TPred a
    Always      :: TPred a -> TPred a
    Eventually  :: TPred a -> TPred a
    After       :: Int -> TPred a -> TPred a

evalLTL' :: TPred a -> Str a -> Str Bool
evalLTL' formulae aStr@(h ::: t) =
    case formulae of
        SP aStrPred     -> aStrPred aStr
        Not aTPred      -> negateStr $ evalLTL' aTPred aStr
        Or phi psi      -> RS.zipWith (box (||)) (evalLTL' phi aStr) (evalLTL' psi aStr)
        And phi psi     -> RS.zipWith (box (&&)) (evalLTL' phi aStr) (evalLTL' psi aStr)
        Implies phi psi -> evalLTL' (Not phi `Or` psi) aStr
        Imminently phi  -> evalLTL' phi (adv t)
        Eventually phi  -> evalLTL' (
                            Imminently phi 
                            `Or` Imminently (Imminently phi)
                            `Or` Imminently (Imminently $ Imminently phi)
                            ) aStr
        Until phi psi   -> error "Not Implemented"
        Always phi      -> RS.zipWith
                                (box (&&))
                                (evalLTL' phi aStr)
                                (evalLTL' (Always phi) (adv t)) -- loops forever.
        After anInt phi -> error "Not Implemented"

evalLTL :: TPred a -> Str a -> Bool
evalLTL formulae aStr = strHead (evalLTL' formulae aStr)

checkLTL :: TPred a -> Str a -> Bool
checkLTL aTPred aStr =
    checkLTL' aTPred aStr (0::Int)
    where
        checkLTL' (Always phi) (h ::: t) counter =
            evalLTL phi (h ::: t)
            && counter > 20 -- eerr?
            || checkLTL' (Always phi) (adv t) (counter + 1)
        checkLTL' (Until phi psi) (h ::: t) counter =
            evalLTL psi (h ::: t)
            || evalLTL phi (h ::: t)
            && (counter > 20
                || checkLTL' (Until phi psi) (adv t) (counter + 1))

        checkLTL' _ _ _ = False






tautology :: TPred a
tautology = SP (\_ -> constStr True)

