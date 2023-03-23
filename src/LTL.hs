{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE GADTs #-}
module LTL where
import Rattus.Stream hiding (const)
import qualified Rattus.Stream as RS
import Evaluators
import Helpers
import Rattus

type StrPred a = Str a -> Str Bool

data TPred a where
    SP  :: StrPred a -> TPred a
    Not :: TPred a -> TPred a
    And :: TPred a -> TPred a -> TPred a
    Or  :: TPred a -> TPred a -> TPred a

    -- etc.

evalLTL' :: TPred a -> Str a -> Str Bool
evalLTL' formulae aStr =
    case formulae of
        SP aStrPred -> aStrPred aStr
        Not aTPred  -> negateStr $ evalLTL' aTPred aStr
        And phi psi -> RS.zipWith (box (&&)) (evalLTL' phi aStr) (evalLTL' psi aStr)
        Or phi psi  -> RS.zipWith (box (||)) (evalLTL' phi aStr) (evalLTL' psi aStr)

    -- etc.


evalLTL :: TPred a -> Str a -> Bool
evalLTL formulae aStr = allTrue  (evalLTL'  formulae aStr)

tautology :: TPred a
tautology = SP (\_ -> constStr True)
mkSP :: Str Bool -> TPred a
mkSP aStr =   SP (const aStr)

