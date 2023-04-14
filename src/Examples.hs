{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PlayArea where
import Test.QuickCheck
import Generators
import Data.Char
import System.Random
import Control.Monad.State
import Rattus.Primitives
import Rattus.Stream
import Helpers
import LTL

type CStrPred a = Str a -> Bool
data CPred a where
    CSP          :: CStrPred a -> CPred a
    CImminently  :: CPred a -> CPred a
    CAlways      :: CPred a -> CPred a
    CEventually  :: CPred a -> CPred a
    COr          :: CPred a -> CPred a -> CPred a
    CAnd         :: CPred a -> CPred a -> CPred a


-- Mocked Example properties

-- genStr :: Arbitrary a  => CPred a -> Gen (Str a)
-- genStr _ = arbitrary :: Gen (Str a)

genIntStr :: CPred Int -> Gen (Str Int)
genIntStr _ = arbitrary :: Gen (Str Int)

genBoolStr :: CPred Bool -> Gen (Str Bool)
genBoolStr _ = arbitrary

genThread :: CPred (Bool, String) -> Gen Thread
genThread _ = arbitrary

sum3 :: Num a => Str a -> a
sum3  _ = -1

prop_sum3_Positive_Positive:: Property
prop_sum3_Positive_Positive =
    let positiveHeadLTL = CSP ((>0) . strHead)
        threePositive =
            positiveHeadLTL
            `CAnd` CImminently positiveHeadLTL
            `CAnd` CImminently (CImminently positiveHeadLTL)
    in
    forAll
        (genIntStr threePositive)
        ((>0) . sum3)

prop_sum10_3each_9total :: Property
prop_sum10_3each_9total =
    let threeHead = CSP ((==0) . strHead)
        threeThrees =
            threeHead
            `CAnd` CImminently threeHead
            `CAnd` CImminently (CImminently threeHead)
    in
    forAll
        (genIntStr threeThrees)
        ((==9) . sum3)

-- perhaps test of imminently satisfying eventually?

type Pressed = Bool
type Light = Bool

turnOn :: Str Pressed -> Str Light
turnOn _ = constStr False


prop_turnOn_ImminentlyPressed_ImminentlyLight :: Property
prop_turnOn_ImminentlyPressed_ImminentlyLight =
    forAll
        (genBoolStr $ CImminently (CSP strHead))
        (evalLTL (Imminently (SP strHead)) . turnOn)

-- CTRL ALT DELETE
-- Always - eventually
type Thread = Str (Bool, String)

-- To do. round robin?
schedule :: [Thread] -> Thread
schedule = head

infinitelyActive :: CPred (Bool, b)
infinitelyActive = CAlways
                        (CEventually
                            (CSP (fst . strHead))
                        )

isInfinitelyActive :: TPred (a, String)
isInfinitelyActive = Always
                        (Eventually
                            (SP ((=="t1") . snd . strHead))
                        )

prop_schedule_infinitelyActive_noDeadLock :: Property
prop_schedule_infinitelyActive_noDeadLock =
    forAll
        (liftM2
            (\a b -> [a, b])
            (genThread infinitelyActive)
            (genThread infinitelyActive)
        )
        $ evalLTL isInfinitelyActive . schedule



negateFalse :: Str Bool -> Str Bool
negateFalse = Rattus.Stream.map (box $ \b -> b || not b)
                        
prop_negateFalse_eventuallyTrue_alwaysTrue :: Property
prop_negateFalse_eventuallyTrue_alwaysTrue =
    forAll
        (genBoolStr $ CEventually (CSP strHead))
        $ evalLTL (Always (SP strHead))




