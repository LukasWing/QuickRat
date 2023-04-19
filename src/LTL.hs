{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE GADTs #-}
module LTL where
import Rattus.Stream hiding (const)
import qualified Rattus.Stream as RS
import Helpers
import Rattus

type StrPred a = Str a -> Bool 

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
    evalLTL' checksLeft formulae aStr@(_ ::: t) =
        checksLeft <= 0 ||
        let evalNext = evalLTL' (checksLeft - 1) 
            eval = evalLTL' checksLeft  
            strTail = adv t
        in
        case formulae of
            SP aStrPred     -> aStrPred aStr
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
            

