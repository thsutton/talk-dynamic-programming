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

-- | Find an optimal solution to a problem given optimal solutions to all
-- sub-problems.
mcmStep
    :: Vector (Int,Int)
    -> (Int,Int)
    -> ((Int,Int) -> (Int, (Int,Int), Vector Int))
    -> (Int, (Int,Int), Vector Int)
mcmStep ms (i,j) get
    | i == j    = let (x,y) = ms V.! i in (0, (x,y), mempty)
    | otherwise = head $ sortBy (compare `on` fsst) $ map calc [i..j-1]
  where
    fsst (x,_,_) = x
    calc s =
        let (lc, (lx,ly), ls) = get (i,s)
            (rc, (rx,ry), rs) = get (s+1,j)
        in (lc + rc + (lx * ly * ry), (lx,ry), (V.singleton s) <> ls <> rs)

-- | Solve a matrix-chain multiplication problem.
mcm :: Vector (Int,Int) -> (Int, (Int,Int), Vector Int)
mcm ms =
    let n     = V.length ms
        param = mcmParam n
        ix    = mcmIx n
        solve = mcmStep ms
    in dp ix param solve (triangular n)

-- * Utility

-- | Number of filled cells in a triangular matrix of size n.
triangular :: Int -> Int
triangular n = n * (n + 1) `div` 2
