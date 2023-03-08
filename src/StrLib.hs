module StrLib (mainStr) where

import Test.QuickCheck
import Rattus.Stream (Str ((:::)))
import Rattus (delay)
import Rattus.Primitives (adv)
makeConstStr :: Str Integer
makeConstStr = 5 ::: delay makeConstStr

getHead ::Str a -> a
getHead aStr = getHead' aStr
        where getHead' (head:::_) = head

getHead5 :: Str a -> [a]
getHead5 aStr = getHead5' 5 [] aStr
        where getHead5' picksLeft accumulator (head:::tail) =
                if picksLeft > 0 then getHead5' (picksLeft - 1) (head:accumulator) (adv tail) else accumulator

-- getHead5Int :: ArbitraryStr -> 
-- getHead5Int :: ArbitraryStr -> [Int]

getHead5Int :: ArbitraryStr -> [Int]
getHead5Int = getHead5Int' 5 []
        where getHead5Int' picksLeft accumulator anArbStr = 
                let (h1:::t1) = unArbitraryStr anArbStr in
                let rest = ArbitraryStr(adv t1) in
                if picksLeft > 0 
                    then getHead5Int' (picksLeft - 1) (h1:accumulator) rest 
                    else reverse accumulator

newtype ArbitraryStr = ArbitraryStr {unArbitraryStr :: Str Int}


instance Arbitrary ArbitraryStr where
    arbitrary = do
        x <- arbitrary:: Gen Int
        xs <- (arbitrary:: Gen ArbitraryStr)
        return (ArbitraryStr (x:::delay (unArbitraryStr xs)))

instance Show ArbitraryStr where
    show = show . getHead5Int


mainStr = do
    print $ getHead5 makeConstStr
    num <- generate (arbitrary::Gen ArbitraryStr)
    print num
    putStrLn "mainStr Done"




