module TypeFromMeetings where
import Helpers 
import Test.QuickCheck
import LTL

data Stamate a = Pass 
                | Fail
                | NextT (a -> Stamate a)

newtype Stamage a = NextG (Gen (Maybe (a, Stamage a)))
-- Nothing means it did not succeed in generating suitable.

type StrPred a = a -> Bool 

-- 1 
mkStamage :: Stamate a -> Stamage a
mkStamage _ = errorNotImplemented

mkStamate :: TPred a -> Stamate a
mkStamate _ = errorNotImplemented

-- 2
suchThat :: Stamage a -> Stamate a -> Stamage a
suchThat _ _ = errorNotImplemented

-- 3 Implement back tracking in v2
and :: Stamate a -> Stamate a -> Stamate a
and _ _ = errorNotImplemented
 
