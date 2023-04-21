module TypesFromMeetings where
--  0
newtype Stamate a = Next (a -> Either Bool (Stamate a)) 
-- Left: check is done with passing is  true false, Right: more checking is needed.
newtype Stamage a = Next (Gen (Maybe (a, Stamage a)))
-- Nothing means it did not succeed

-- make StrPred a:: a -> Bool 
-- 1 
mkStamage :: Stamate a -> Stamage a
mkStamage _ = errorNotImplemented

mkStamate :: Int -> TPred a -> Stamate a
mkStamate n p = 

-- 2
suchThat :: Stamage a -> Stamate a -> Stamage a
suchThat _ _ = errorNotImplemented

-- 3 Implement back tracking in 2

and :: Stamate a -> Stamate a -> Stamate a
and _ _ = errorNotImplemented
 
