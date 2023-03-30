{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE GADTs #-}
module LTL where
import Rattus.Stream hiding (const)
import qualified Rattus.Stream as RS
import Evaluators
import Helpers
import Rattus

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
evalLTL' formulae aStr@(h:::t) =
    case formulae of
        SP aStrPred     -> aStrPred aStr
        Not aTPred      -> negateStr $ evalLTL' aTPred aStr
        Or phi psi      -> RS.zipWith (box (||)) (evalLTL' phi aStr) (evalLTL' psi aStr)
        And phi psi     -> RS.zipWith (box (&&)) (evalLTL' phi aStr) (evalLTL' psi aStr)
        Implies phi psi -> evalLTL' (Not phi `Or` psi) aStr
        Eventually phi  -> error "Not Implemented" -- some suffix
        Until phi psi   -> error "Not Implemented"
        Imminently phi  -> error "Not Implemented"
        Always phi      -> RS.zipWith 
                                (box (&&)) 
                                (evalLTL' phi aStr) 
                                (evalLTL' (Always phi) (adv t)) -- loops forever.
        After anInt phi -> error "Not Implemented"

evalLTL :: TPred a -> Str a -> Bool
evalLTL formulae aStr = allTrue (evalLTL' formulae aStr)

tautology :: TPred a
tautology = SP (\_ -> constStr True)

mkSP :: Str Bool -> TPred a
mkSP aStr = SP (const aStr)


