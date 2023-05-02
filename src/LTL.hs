{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE GADTs #-}
module LTL where
import Rattus.Stream hiding (filter, const)
import Rattus
import Helpers
import Functions

tautology :: TPred a
tautology = SP (const True)

contradiction :: TPred a
contradiction = SP (const False)



evalLTL :: TPred a -> Str a -> Bool
evalLTL = evalLTL' 20 

evalLTL' :: Int -> TPred a -> Str a -> Bool
evalLTL' checksLeft formulae aStr@(_ ::: t) =
    checksLeft <= 0 || case formulae of
        SP elementPred  -> elementPred $ strHead aStr
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
    
    where   evalNext = evalLTL' (checksLeft - 1)
            eval = evalLTL' checksLeft
            strTail = adv t


altsOddOrEven :: TPred Int
altsOddOrEven = Always $
    (SP even `And` Imminently (SP odd))
    `Or`
    (SP odd `And` Imminently (SP even))
    
oddEvenLTL :: TPred Int
oddEvenLTL = SP odd `And` Imminently evenOddLTL
evenOddLTL :: TPred Int
evenOddLTL = SP even `And` Imminently oddEvenLTL

alternatesEvenOdd :: Str Int -> Bool
alternatesEvenOdd = evalLTL evenOddLTL

alternatesOddEven :: Str Int -> Bool
alternatesOddEven = evalLTL oddEvenLTL
