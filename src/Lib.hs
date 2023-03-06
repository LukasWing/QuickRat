module Lib
    ( someFunc
    ) where
import Test.QuickCheck
import Rattus
import Rattus.Stream (Str)

gcd' ::  Int -> Int -> Int
gcd' a b
    | b == 0 = a
    | otherwise = gcd' b (a `mod` b)


tGen :: Gen Int
tGen = choose (1, 10)

prop_gcdIs1For17TinyInt :: Property
prop_gcdIs1For17TinyInt =
    forAll tGen (\aTinyInt -> gcd' aTinyInt 17 == 1)

prop_positive :: Int -> Int -> Property
prop_positive a b =
    (a > 0 && b > 0) ==> (gcd' a b > 0)

data MyBool = MyTrue 
            | MyFalse 
    deriving (Show)


instance Arbitrary MyBool where
    arbitrary = frequency 
        [(4, return MyTrue), (1, return MyFalse)]

newtype MyList = MyList{getList :: [Int]} 
    deriving (Show)

instance Arbitrary MyList where
    arbitrary = frequency [(p a b, return (linearFunction a b)) | a <- [-10..10], b <- [0..10]] 
        where linearFunction a b =  MyList (map (\x -> a * x + b) [-10..10])
              p a b = max (a * b + 1) 1

{-
inst arbitrary
do x<- 
recurse
peel off
-}

someFunc :: IO ()
someFunc = do
    putStrLn "myFuncs"
    quickCheck prop_positive    
    quickCheck prop_gcdIs1For17TinyInt
    sample (arbitrary::Gen MyBool) 
    sample (arbitrary::Gen MyList) 

