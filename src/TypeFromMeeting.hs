module TypeFromMeetings where
import Helpers 
import Test.QuickCheck
import LTL
--  0
newtype Stamate a = Next (a -> Either Bool (Stamate a)) 
-- Left: check is done when passing is true, false it it failed, Right: more checking is needed.

-- More expressive type:
newtype Stamate' a = NextT (a -> Checker a)
data Checker a = Pass 
                | Fail 
                | Stamate' a 

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
 
