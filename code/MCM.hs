module MCM where

import           Data.Vector        (Vector)
import qualified Data.Vector        as V

import           DynamicProgramming

-- * Matrix-Chain Multiplication
--
-- $ This code solves the matrix-chain multiplication scheduling problem: it
--   determines to optimal order in which to compute a chain of matrix
--   multiplications.

-- | The index for a matrix-chain multiplication sub-problem.
mcmIx :: Size -> (Int, Int) -> Index
mcmIx n (i,j) = go n 0 (i,j)
  where
    go :: Int -> Int -> (Int, Int) -> Int
    go m r (x,y)
        | (y - x) == 0 = r + x
        | otherwise    = go (m - 1) (r + m) (x, y - 1)

-- | The matrix-chain multiplication sub-problem for an `Index`.
mcmParam :: Size -> Index -> (Int, Int)
mcmParam n i = go n 0 i
  where
    go :: Int -> Int -> Int -> (Int, Int)
    go m r x
        | x < m     = (x,x + r)
        | otherwise = go (m - 1) (r + 1) (x - m)

-- | Solve a matrix-chain multiplication problem.
mcm :: Vector (Int,Int) -> Int
mcm ms =
    let n     = V.length ms
        sz    = triangular n
        param = mcmParam n
        ix = mcmIx n
        solve (i,j) get = 0
    in dp ix param solve sz

-- * Utility

-- | Number of filled cells in a triangular matrix of size n.
triangular :: Int -> Int
triangular n = n * (n + 1) `div` 2
