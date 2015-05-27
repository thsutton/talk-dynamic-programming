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

-- | A problem involves bracketing a sequence of matrices.
type Problem = (Int, Int)

-- | A solution includes a cost, dimensions of the resulting matrix, and a list of
-- splitting points (in preorder).
type Solution = (Int, (Int,Int), Vector Int)

-- | The index for a matrix-chain multiplication sub-problem.
--
-- The size of the problem is the number of matrices in the chain to multiply
-- and the pair is a range 0<=i<=j<size of matrices. The result is the index
-- of the tableau cell for the problem.
mcmIx :: Size -> Problem -> Index
mcmIx n (i,j) = i + sumFromTo (n-j+i+1) n

-- | The matrix-chain multiplication sub-problem for an `Index`.
--
-- The size of the problem is the number of matrices in the chain to multiply,
-- the index is the tableau cell for the problem, and the pair is a range
-- 0<=i<=j<size of matrices.
mcmParam :: Size -> Index -> Problem
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
    -> Problem
    -> (Problem -> Solution)
    -> Solution
mcmStep ms (i,j) get
    | i == j    = (0, ms V.! i, mempty)
    | otherwise = minimumBy (compare `on` fsst) $ map calc [i..j-1]
  where
    calc s =
        let (lc, (lx,ly), ls) = get (i,s)
            (rc, ( _,ry), rs) = get (s+1,j)
        in (lc + rc + (lx * ly * ry), (lx,ry), V.singleton s <> ls <> rs)

-- | Solve a matrix-chain multiplication problem.
mcm :: Vector (Int,Int) -> (Int, (Int,Int), Vector Int)
mcm ms = let n = V.length ms
         in dp (mcmIx n) (mcmParam n) (mcmStep ms) (sumFromTo 1 n)

-- * Utility

-- | Sum the numbers in the sequence [n..m]. This is the triangular numbers
-- when n=1.
sumFromTo n m = (m * (m+1) `div` 2) - ((n-1) * n `div` 2)

fsst :: (a,b,c) -> a
fsst (x,_,_) = x
