{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE GADTs #-}
module LTL where
import Rattus.Stream hiding (const)
import qualified Rattus.Stream as RS
import Evaluators
import Helpers
import Rattus

type StrPred a = Str a -> Bool -- SM

tautology :: TPred a
tautology = SP (const True)

contradiction :: TPred a
contradiction = SP (const False)
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


evalLTL :: TPred a -> Str a -> Bool
evalLTL = evalLTL' 20 where
    evalLTL' :: Int -> TPred a -> Str a -> Bool
    evalLTL' checksLeft formulae aStr@(h ::: t) =
        
        checksLeft <= 0 ||
        let evaln = evalLTL' (checksLeft - 1) 
            eval = evalLTL' checksLeft  
        in
        case formulae of
            SP aStrPred     -> aStrPred aStr
            Not aTPred      -> not $ eval aTPred aStr
            Or phi psi      -> eval phi aStr || eval psi aStr
            And phi psi     -> eval phi aStr && eval psi aStr
            Implies phi psi -> eval (Not phi `Or` psi) aStr
            Imminently phi  -> evaln phi (adv t)
            Eventually phi  -> eval phi aStr || evaln (Eventually phi) (adv t)
            Until phi psi   -> error "Not Implemented"
            Always phi      -> eval phi aStr && evaln (Always phi) (adv t)
            After anInt phi -> error "Not Implemented"

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



