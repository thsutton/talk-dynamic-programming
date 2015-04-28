module DynamicProgramming where

import           Control.Applicative
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

type Index = Int
type Size = Int

-- | Use dynamic programming to solve a problem.
dp :: (param -> Index)
   -> (Index -> param)
   -> (param -> (param -> solution) -> solution)
   -> Size
   -> solution
dp index param step n =
    let solve subs =
            let i = V.length subs
                get p = subs V.! (index p)
            in step (param i) get
        tableau = V.constructN n solve
    in V.last tableau

-- * Matrix-chain Multiplication

-- | The index for a matrix-chain multiplication sub-problem.
mcmIx
    :: Int          -- ^ Size of the whole problem.
    -> (Int, Int)   -- ^ Particular sub-problem.
    -> Index
mcmIx n (i,j) = 0

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
        param x = let i = 0 ; j = 0 in (i,j)
        ix (i,j) = 0
        solve (i,j) get = 0
    in dp ix param solve n

-- * String Edit Distance

-- | The index for a string edit distance sub-problem.
editIx
    :: Int
    -> a
    -> Index
editIx n p = error ""

editParam
    :: Int
    -> Index
    -> a
editParam n i = error ""

data Op
    = Del
    | Ins Char
    | Sub Char

editDistance
    :: Vector Char
    -> Vector Char
    -> [Op]
editDistance s t =
    let n = V.length s
        m = V.length t
        ix = editIx m
        param = editParam m
        solve i get = undefined
    in dp ix param solve (n * m)
