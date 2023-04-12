{-# OPTIONS_GHC -Wno-missing-signatures #-}
module PlayArea(
    run,
    pop,
    push,
    Stack,
    pop',
    push'
) where
import Test.QuickCheck
import Control.Monad.Writer
import Generators
import Data.Char
import System.Random
import Control.Monad.State
import Rattus.Primitives
import Debug.Trace

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

run1 :: IO ()
run1 = do
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
    -- sample (arbitrary:: Gen (Str (Int,String)))
    sample evenOddGen
    sample oddEvenGen
    -- print $ evenOddGen evenOdd

addStuff :: [Char] -> [Char]
addStuff = do
    l <- length
    a <- take (3 * l) . cycle
    hasCap <- any isUpper
    hasNum <- any isDigit
    return $ a
        ++ ". Length is now: "
        ++ show (3*l)
        ++ show hasCap
        ++ show hasNum

twoCoins :: StdGen -> (Bool, Bool)
twoCoins gen =
    let (c1, gen') = random gen
        (c2, _) = random gen'
    in (c1, c2)

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (h:newStack) = (h, newStack)
pop [] = error "Nothing to pop"

push ::  Int -> Stack -> ((), Stack)
push newElement aStack = ((), newElement:aStack)

pop' :: State Stack Int
pop' = state $ \(h:newStack) -> (h, newStack)

push' :: Int -> State Stack ()
push' newElement = state $ \aStack -> ((), newElement:aStack)



prop_stackPushedIsPopped anInt =
    let (_, newStack) = push anInt [] in
    let (expectedAnInt, _) = pop newStack in
    anInt == expectedAnInt

stackManip:: Int -> State Stack Int
stackManip anInt = do
    push' anInt
    pop'

prop_stackPushedIsPopped' anInt =
    let (expectedInt, _) = runState (stackManip anInt) []
    in expectedInt == anInt

data Z = Z {a::Float, b::Float}
genComplex = arbitrary >>= (\a ->
             arbitrary >>= (\b ->
             return Z {a=a, b=b}))



mapL :: Box (a -> b) -> O a -> O b
mapL f inpF = delay (unbox f (adv inpF))

traceTest :: Int -> Int
traceTest a = 
    let f x = 3 * x in
    trace ("calling f with x = " ++ show a) (f (a::Int))



runAsync = do
    quickCheck $ \inpF -> adv (mapL (box id) (delay (inpF::Char))) == adv (delay inpF)
    print (traceTest 2)
    putStrLn "Run Async Done"



run = do
    print $ addStuff "Hey"
    print $ twoCoins (mkStdGen 100)
    let (exp,_) = runState (stackManip 4) [7]
    quickCheck prop_stackPushedIsPopped
    quickCheck prop_stackPushedIsPopped'
    runAsync
    print "Done"
















