{-# LANGUAGE GADTs #-}
module Types (
    TPred(..),
    Stamate(..),
    Stamage(..),
) where 
import Test.QuickCheck (Gen)

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

