{-# OPTIONS_GHC -Wno-orphans #-}
module Helpers where
import Rattus.Stream
import Rattus

class AlmostEq a where
    (=~=) :: a -> a -> Bool

instance (Show a) => Show (Str a) where
    show aStr =  "Str: " ++ (show . strTake 20) aStr

instance (Eq a) => AlmostEq (Str a) where
    stream1 =~= stream2 = strTake 5 stream1 == strTake 5 stream2

strTake :: Integer -> Str a -> [a]
strTake n aStr =
    let strTake' picksLeft accumulator (h:::t) =
            if picksLeft > 0
                then strTake' (picksLeft - 1) (h:accumulator) (adv t)
                else reverse accumulator
    in strTake' n [] aStr

strHead :: Str a -> a
strHead  (h:::_) = h

constStr :: a -> Str a
constStr v = v ::: delay (constStr v)