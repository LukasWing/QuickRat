{-# OPTIONS_GHC -Wno-orphans #-}
module Helpers where
import Rattus.Stream
import Rattus.Stream as RS

import Rattus
import qualified Rattus.Stream as Stream

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

strGet :: Int -> Str a -> a 
strGet n aStr = last $ strTake (fromIntegral (n + 1)) aStr 

strHead :: Str a -> a
strHead  (h:::_) = h

constStr :: a -> Str a
constStr v = v ::: delay (constStr v)

negateStr :: Str Bool  -> Str Bool
negateStr = RS.map (box not)

applyN :: Int -> (a -> a) -> a -> a
applyN n f = (!! n ) . iterate f

strExtend :: [a] -> Str a
strExtend [h] = constStr h
strExtend (h:t) = h ::: delay (strExtend t) 
strExtend [] = error "No value in list"

errorNotImplemented = error "Not Implemented"

