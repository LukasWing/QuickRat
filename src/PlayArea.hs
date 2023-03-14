module PlayArea( run
) where

import Test.QuickCheck
import Control.Monad.Writer 
import StreamGen

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

data MyNested = MyNested {description::String, content::MyList} deriving (Show)

instance Arbitrary MyNested where
    arbitrary = do
        aString <- arbitrary
        aMyList <- arbitrary
        return MyNested {description=aString, content=aMyList}

quicksort' [] = []
quicksort' (p:xs) =
    let smaller =  filter (< p) xs
        larger =  filter (>= p) xs
    in quicksort' smaller ++ [p] ++ quicksort' larger

testSort sorter = 
    let isSorted (x1:x2:xs) = x1 <= x2 && isSorted (x2:xs)
        isSorted _ = True 
    in all (isSorted . sorter) [[],[1],[1,2],[2,1],[-1,1]]

logNumber :: Int -> Writer [String] Int 
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int 
multWithLog = do 
    a <- logNumber 3
    b <- logNumber 5
    return (a*b)

myRev :: [Int] -> [Int] 
myRev = foldl (\reverseList element -> element:reverseList) [] 

run :: IO ()
run = do
    quickCheck (\x -> fromIntegral (x::Int) < 100)
    quickCheck (\xs -> reverse xs == myRev xs)
    print $ runWriter multWithLog
    putStrLn "myFuncs"
    quickCheck prop_positive    
    quickCheck prop_gcdIs1For17TinyInt
    sample (arbitrary::Gen MyBool) 
    sample (arbitrary::Gen MyList) 
    sample (arbitrary::Gen MyNested)
    -- print MyNested {description="1", content=MyList[1,2]}
    -- sample (scale (*33) (arbitrary:: Gen (QStr Int)))
    sample (arbitrary:: Gen (Str (Int,String)))

  


