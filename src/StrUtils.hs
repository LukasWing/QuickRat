module StrUtils (
    getHead, 
    constStr, 
    makeMyConstStr, 
    makeConstStr,
    showStr, 
    strTake,
) where

import Test.QuickCheck
import Rattus.Stream (Str ((:::)))
import Rattus (delay)
import Rattus.Primitives (adv)

constStr :: t -> Str t
constStr v = v ::: delay (constStr v)

getHead ::Str a -> a
getHead (h:::_) = h


strTake :: (Ord t, Num t) => t -> Str a -> [a]
strTake n = strTake' n [] 
        where strTake' picksLeft accumulator (head:::tail) =
                if picksLeft > 0 
                    then strTake' (picksLeft - 1) (head:accumulator) (adv tail) 
                    else reverse accumulator

showStr :: Show a => Str a -> String
showStr aStr = show $ strTake 5 aStr

makeConstStr :: Str Int
makeConstStr = 5 ::: delay makeConstStr

makeMyConstStr :: Int -> Str Int
makeMyConstStr n = n ::: delay (makeMyConstStr n)




