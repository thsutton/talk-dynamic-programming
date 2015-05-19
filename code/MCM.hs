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
mcmIx
    :: Int          -- ^ Size of the whole problem.
    -> (Int, Int)   -- ^ Particular sub-problem.
    -> Index
mcmIx n (i,j) = go n 0 (i,j)
  where
    go :: Int -> Int -> (Int, Int) -> Int
    go 0 r (_,_) = r
    go m r (x,y) =
        if (y - x) == 0
            then r + x
            else go (m - 1) (r + m) (x, y - 1)

-- | The matrix-chain multiplication sub-problem for an `Index`.
mcmParam
    :: Int          -- ^ Size of the whole problem.
    -> Index        -- ^ DP tableau index.
    -> (Int, Int)
mcmParam n i = go n 0 i
  where
    go :: Int -> Int -> Int -> (Int, Int)
    go m r x
        | x < m = (x,x + r)
        | otherwise = go (m - 1) (r + 1) (x - m)

-- | Solve a matrix-chain multiplication problem.
mcm :: Vector (Int,Int) -> Int
mcm ms =
    let n = V.length ms
        sz = (n * (n + 1)) `div` 2
        param = mcmParam n
        ix = mcmIx n
        solve (i,j) get = 0
    in dp ix param solve sz

