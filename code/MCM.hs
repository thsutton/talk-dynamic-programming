module MCM where

import           Data.Function
import           Data.List
import           Data.Monoid
import           Data.Vector        (Vector)
import qualified Data.Vector        as V

import           DynamicProgramming

-- * Matrix-Chain Multiplication
--
-- $ This code solves the matrix-chain multiplication scheduling problem: it
--   determines to optimal order in which to compute a chain of matrix
--   multiplications.

-- | The index for a matrix-chain multiplication sub-problem.
--
-- The size of the problem is the number of matrices in the chain to multiply
-- and the pair is a range 0<=i<=j<size of matrices. The result is the index
-- of the tableau cell for the problem.
mcmIx :: Size -> (Int, Int) -> Index
mcmIx n (i,j) = i + sumFromTo (n-j+i+1) n

-- | The matrix-chain multiplication sub-problem for an `Index`.
--
-- The size of the problem is the number of matrices in the chain to multiply,
-- the index is the tableau cell for the problem, and the pair is a range
-- 0<=i<=j<size of matrices.
mcmParam :: Size -> Index -> (Int, Int)
mcmParam n = go n 0
  where
    go :: Int -> Int -> Int -> (Int, Int)
    go m r x
        | x < m     = (x,x + r)
        | otherwise = go (m - 1) (r + 1) (x - m)

-- | Find an optimal solution to a problem given optimal solutions to all
-- sub-problems.
mcmStep
    :: Vector (Int,Int)
    -> (Int,Int)
    -> ((Int,Int) -> (Int, (Int,Int), Vector Int))
    -> (Int, (Int,Int), Vector Int)
mcmStep ms (i,j) get
    | i == j    = (0, ms V.! i, mempty)
    | otherwise = minimumBy (compare `on` fsst) $ map calc [i..j-1]
  where
    calc s =
        let (lc, (lx,ly), ls) = get (i,s)
            (rc, (rx,ry), rs) = get (s+1,j)
        in (lc + rc + (lx * ly * ry), (lx,ry), V.singleton s <> ls <> rs)

-- | Solve a matrix-chain multiplication problem.
mcm :: Vector (Int,Int) -> (Int, (Int,Int), Vector Int)
mcm ms = let n = V.length ms
         in dp (mcmIx n) (mcmParam n) (mcmStep ms) (sumFromTo 1 n)

-- * Utility

sumFromTo n m
    | n == 1    = m * (m+1) `div` 2
    | otherwise = sumFromTo 1 m - sumFromTo 1 (n-1)

fsst :: (a,b,c) -> a
fsst (x,_,_) = x
