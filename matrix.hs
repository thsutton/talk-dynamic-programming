{-# LANGUAGE TemplateHaskell #-}

module Matrix where

import Control.Applicative
import System.Exit

import Test.QuickCheck
import Test.QuickCheck.All

-- | Convert the index of a sub-problem into tableau coordinates.
coords
    :: Int -- ^ Problem size.
    -> Int -- ^ Sub-problem index.
    -> (Int, Int)
coords n i = w n 0 i
  where
    w :: Int -> Int -> Int -> (Int, Int)
    w m r x
        | x < m     = (x,x)
        | otherwise = succ <$> w (m - 1) (r + 1) (x - m)

-- | Convert the tableaux coordinates of a sub-problem into an index.
index :: Int -> (Int, Int) -> Int
index n (i,j)
    | i == j = i
    | otherwise = 9999
    
triangular :: Int -> Int
triangular n =
    let adj = if odd n then 1 else 0
    in (n * n) `quot` 2 + (n `quot` 2) + adj

-- * Tests

type IxTest i = (i, [(i, (i, i))])

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

check_index :: IxTest Int -> (NonNegative Int, NonNegative Int) -> Property
check_index (n, l) (NonNegative i, NonNegative j) =
    (i < n && j < n) ==> (Just (i,j)) == (lookup (index n (i,j)) l)

check_coords :: IxTest Int -> NonNegative Int -> Property
check_coords (n, l) (NonNegative x) =
    (x < (triangular n)) ==> Just (coords n x) == (lookup x l)

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

