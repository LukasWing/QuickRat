{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE GADTs #-}
module LTL where
import Rattus.Stream hiding (const)
import Evaluators
import Helpers

type StrPred a = Str a -> Str Bool

data TPred a where
    SP  :: StrPred a -> TPred a
    Not :: TPred a -> TPred a
    And :: TPred a -> TPred a -> TPred a
    -- etc.

evalLTL' :: TPred a -> Str a -> Str Bool
evalLTL' formulae aStr =
    case formulae of
        SP aStrPred ->  aStrPred aStr
        Not aTPred -> negateStr $ evalLTL' aTPred aStr
        -- Not (SP spf) -> evalLTL (SP (negateStr . spf)) aStr
        -- Not (Not aStrPred)  -> evalLTL aStrPred aStr
        -- Not tpF -> evalLTL tpF 
        And _ _ -> constStr True
    -- etc.

evalLTL :: TPred a -> Str a -> Bool
evalLTL formulae aStr = allTrue  (evalLTL'  formulae aStr)

tautology = SP (\_ -> constStr True)


