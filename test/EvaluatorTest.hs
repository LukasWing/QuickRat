{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- your code here

module EvaluatorTest (
    runTests
) where
import Generators
import Evaluators
import Helpers
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Rattus.Stream hiding (const)
import Test.QuickCheck.Monadic ()
import Data.Maybe (isNothing)
import LTL
import Control.Monad (liftM2, liftM4)
import Functions

prop_seesConst :: Int -> Bool
prop_seesConst value = isConstCheck (constStr (value::Int))

prop_strProbEqSameStrTrue :: Eq a => Str a -> Bool
prop_strProbEqSameStrTrue aStr = aStr `strProbEq` aStr

prop_stamateRun'_anyStream_constsOK :: Str Bool -> NonNegative Int -> Bool
prop_stamateRun'_anyStream_constsOK aStr (NonNegative {getNonNegative=i}) =
    not (stamateRun' aStr Fail 0)
    && stamateRun' aStr (NextT (const Fail)) 0
    && stamateRun' aStr Pass 0
    && stamateRun' aStr (NextT (const Pass)) i

prop_stamateRun'_extendedStreams_constsOK :: Bool
prop_stamateRun'_extendedStreams_constsOK =
    stamateRun' (strExtend [1]) (isConstSM (Just 2)) 0
    && stamateRun' (strExtend [2,1,1,1,1]) (isConstSM (Just 2)) 1
    && stamateRun' (strExtend [2,2,1,1,1]) (isConstSM (Just 2)) 2
    && not (stamateRun' (strExtend [2,2,1,1,1]) (isConstSM (Just 1)) 2)
    && not (stamateRun' (strExtend [2,2,1,1,1]) (isConstSM (Just 2)) 3)

prop_suchThatT_simpleConst_GenOfNothing :: Property
prop_suchThatT_simpleConst_GenOfNothing =
    forAll
        (let (NextG inside) = constOfG True `suchThatT` isConstSM (Just False)
        in inside)
        isNothing

prop_suchThatT_arbitraryAndContradiction_GenOfNothing :: Property
prop_suchThatT_arbitraryAndContradiction_GenOfNothing =
    forAll
        (let (NextG inside) = arbitraryStamage `suchThatT` (contradictionSM:: Stamate Int)
        in inside)
        isNothing

prop_suchThatT_sameConst2_2strsOnly :: Property
prop_suchThatT_sameConst2_2strsOnly =
    forAll
        (stamageRun $ constOfG True `suchThatT` isConstSM (Just True))
        $ isConstVal True

prop_suchThatT_oddEvenTautolgy_IsOddEven :: Property
prop_suchThatT_oddEvenTautolgy_IsOddEven =
    forAll
        (stamageRun $ oddEven `suchThatT` tautologySM)
        (\aStr -> collect aStr $ alternatesOddEven aStr)

prop_suchThatT_oddEvenKeepOnlyPositive_IsOddEven :: Property
prop_suchThatT_oddEvenKeepOnlyPositive_IsOddEven =
    forAll
        (stamageRun $ oddEven `suchThatT` isPositive)
        (\aStr -> collect aStr $ alternatesOddEven aStr)

prop_mkStamage_trueConst_isTrueConst :: Property
prop_mkStamage_trueConst_isTrueConst =
    forAll
        (stamageRun $ mkStamage $ isConstSM (Just True))
        $ isConstVal True

-- prop_mkStamage_12345678_is12345678 :: Property
prop_mkStamage_constOf8_isConstOf8 :: Property
prop_mkStamage_constOf8_isConstOf8 =
    forAll
        (stamageRun $ mkStamage $ isConstSM (Just (8::Int)))
        $ isConstVal 8

prop_mkStamate_x1EvenX2Odd_holdOnStreams :: Bool
prop_mkStamate_x1EvenX2Odd_holdOnStreams =
     stamateRun
        (strExtend [2,1])
        (mkStamate $ SP even `And` Imminently (SP odd))

x1Even :: Stamate Int
x1Even = NextT (\x -> if even x then Pass else Fail)

x2Odd :: Stamate Int
x2Odd = NextT (\_ -> NextT (\x1 -> if odd x1 then Pass else Fail))

qAndp :: Stamate Int
qAndp = NextT (\x1 -> if even x1
                        then NextT (\x2 -> if odd x2 then Pass else Fail)
                        else Fail)

prop_andT'_x1Evenx2Odd_23Passes :: Property
prop_andT'_x1Evenx2Odd_23Passes =
    forAll
        (liftM2 (,) oddGen evenGen)
        $ \(oddN, evenN) ->
            let NextT firstCheck = x1Even `andT'` x2Odd --qAndp
                NextT secondCheck = firstCheck evenN
                NextT thirdCheck = secondCheck oddN
            in  show (firstCheck evenN) == "N"
                && show (secondCheck evenN) == "N"
                && show (thirdCheck oddN) == "P"

prop_andT'_x2Oddx1Even_23Passes :: Property
prop_andT'_x2Oddx1Even_23Passes =
    forAll
        (liftM2 (,) oddGen evenGen)
        $ \(oddN, evenN) ->
            let NextT firstCheck = x2Odd `andT'` x1Even
                NextT secondCheck = firstCheck evenN
            in  show (firstCheck evenN) == "N"
                && show (secondCheck oddN) == "P"

prop_andT'_x2Oddx1Even_oddFails :: Property
prop_andT'_x2Oddx1Even_oddFails =
    forAll
        oddGen
        $ \oddN ->
            let NextT firstCheck = x2Odd `andT'` x1Even
            in  show (firstCheck oddN) == "F"

prop_mkStamate_x2isOddex1isEvenAndPositive :: Property
prop_mkStamate_x2isOddex1isEvenAndPositive =
    forAll
        (liftM4 (,,,) (abs <$> evenGen) oddGen arbitrary arbitrary)
        $ \(evenPositiveN, oddN, x1Fulfils, randomN) -> stamateRun
            (strExtend $ if x1Fulfils
                            then [evenPositiveN, randomN]
                            else [randomN, oddN]
            )
            (mkStamate $
                Imminently (SP odd) `Or` (SP (>=0) `And` SP even)
            )

prop_mkStamate_afterNAlwaysOdd :: Positive Int -> Bool
prop_mkStamate_afterNAlwaysOdd (Positive {getPositive=pN}) =
    let n = (pN `mod` 10) in
    stamateRun
        (strExtend $ replicate n (2::Int) ++ [1])
        $ mkStamate (After n  (Always (SP odd))
                    `And`
                    Not (After (n-1) (SP odd)))

-- prop_mkStamate_afterNAlwaysOdd' :: Positive Int -> Bool
prop_mkStamate_afterNAlwaysOdd'  =
    stamateRun
        (strExtend [2, 1])
        $ mkStamate (Not (Not (After 1 (SP odd))))


prop_mkStamate_eventually :: Positive Int -> Bool
prop_mkStamate_eventually (Positive {getPositive=pN}) =
    let n = (pN `mod` 10) in
    stamateRun
        (strExtend $ replicate n (2::Int) ++ [1,-1])
        $ mkStamate (Eventually (SP (==1)))


return []
runTests :: IO Bool
runTests = $quickCheckAll




