{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE GADTs #-}
module LTL where
import Rattus.Stream hiding (const)
import Evaluators

type StrPred a = Str a -> Str Bool
data TPred a where
    SP  :: StrPred a -> TPred a
    And :: TPred a -> TPred a -> TPred a
    Not :: TPred a -> TPred a

evalLTL :: TPred a -> Str a -> Bool
evalLTL formulae aStr =
    case formulae of
        SP aStrPred ->  (allTrue . aStrPred) aStr
        And _ _ -> True
        Not _ -> True



