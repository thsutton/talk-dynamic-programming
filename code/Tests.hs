{-# LANGUAGE TemplateHaskell #-}

module Tests where

import Control.Applicative
import System.Exit

import Test.QuickCheck
import Test.QuickCheck.All

import DynamicProgramming
import MCM

-- * Tests

type IxTest i = (i, [(i, (i, i))])

check_index :: IxTest Int -> (NonNegative Int, NonNegative Int) -> Bool
check_index (n, l) (NonNegative i, NonNegative j) =
    let i_n = i `mod` n
        j_n = j `mod` n
        i' = min i_n j_n
        j' = max i_n j_n
    in (Just (i',j')) == (lookup (mcmIx n (i',j')) l)

check_coords :: IxTest Int -> NonNegative Int -> Property
check_coords (n, l) (NonNegative x) =
    (x < (triangular n)) ==> Just (mcmParam n x) == (lookup x l)

-- ** Test data

test_1 :: IxTest Int
test_1 = (1,
    [ (0, (0, 0))
    ])

test_3 :: IxTest Int
test_3 = (3,
    [ ( 0, (0, 0)) , ( 1, (1, 1)) , ( 2, (2, 2))
    , ( 3, (0, 1)) , ( 4, (1, 2))
    , ( 5, (0, 2))
    ])

test_6 :: IxTest Int
test_6 = (6,
    [ ( 0, (0, 0)) , ( 1, (1, 1)) , ( 2, (2, 2)) , ( 3, (3, 3)) , ( 4, (4, 4)) , ( 5, (5, 5))
    , ( 6, (0, 1)) , ( 7, (1, 2)) , ( 8, (2, 3)) , ( 9, (3, 4)) , (10, (4, 5))
    , (11, (0, 2)) , (12, (1, 3)) , (13, (2, 4)) , (14, (3, 5))
    , (15, (0, 3)) , (16, (1, 4)) , (17, (2, 5))
    , (18, (0, 4)) , (19, (1, 5))
    , (20, (0, 5))
    ])

-- ** Properties

prop_index_1 = check_index test_1

prop_coords_1 = check_coords test_1

prop_index_3 = check_index test_3

prop_coords_3 = check_coords test_3

prop_index_6 = check_index test_6

prop_coords_6 = check_coords test_6

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
    s <- runTests
    if s
        then exitSuccess
        else exitFailure
