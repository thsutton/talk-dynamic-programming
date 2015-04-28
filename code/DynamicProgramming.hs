{-# LANGUAGE ViewPatterns #-}

-- | Description: Framework for implementing dynamic programming algorithms.
--
--   This module provides a framework for implementing dynamic programming
--   algorithms by constructing a tableau. The caller provides implementations
--   of an isomorphism between the natural numbers and sub-problems, ordered
--   according to dependencies between problems, and a step function and the
--   code in this module does the rest.
module DynamicProgramming where

import           Control.Applicative
import           Control.Arrow
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

-- | An `Index` in a tableaux identifies a particular sub-problem.
type Index = Int

-- | The `Size` of a tableaux counts the total number of sub-problems to be
-- solved.
type Size = Int

-- | Use dynamic programming to solve a problem.
dp :: (param -> Index) -- ^ Find the index for a particular sub-problem.
   -> (Index -> param) -- ^ Find the sub-problem at a paricular index.
   -> (param -> (param -> solution) -> solution) -- ^ Solve a sub-problem.
   -> Size -- ^ Size of the tableau to construct.
   -> solution
dp index param step n =
    let solve subs =
            let i = V.length subs
                get p = subs V.! (index p)
            in step (param i) get
        tableau = V.constructN n solve
    in V.last tableau

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
        param = mcmParam n
        ix = mcmIx n
        solve (i,j) get = 0
    in dp ix param solve n

-- * String Edit Distance
--
-- $ This code implementes the Wagner-Fischer algorithm (also know by other
--   names) to calculate the least-cost edit script which converts on string
--   into another.

-- | Find the `Index` for a string edit distance sub-problem.
editIx
    :: Size
    -> (Int, Int)
    -> Index
editIx n (x,y) = (x * n) + y

-- | Find the string edit distance sub-problem located at an `Index`.
editParam
    :: Size
    -> Index
    -> (Int, Int)
editParam n i = i `quotRem` n

data Op
    = Del Int Char
    | Ins Int Char
    | Sub Int Char

del :: Int -> Char -> Char -> Op
del i c _ = Del i c

ins :: Int -> Char -> Char -> Op
ins i _ c = Ins i c

sub :: Int -> Char -> Char -> Op
sub i _ c = Sub i c

-- | Used to calculate the position to be affected by an operation.
offset :: Op -> Int
offset (Del _ _) = 0
offset (Ins _ _) = 1
offset (Sub _ _) = 1

editDistance
    :: Vector Char
    -> Vector Char
    -> (Int, [Op])
editDistance s t =
    let m = V.length s
        n = V.length t
        ix    = editIx n
        param = editParam n
        position = sum . fmap (maybe 1 offset)
        -- Extend a solution with an additional constructor.
        op f s' t' p =
            let c = f (position . snd $ p) s' t' in succ *** (Just c :) $ p
        -- Solve a sub-problem.
        solve (        0,         0) get = (0, mempty)
        solve (        0, pred -> y) get = op del (s V.! y) ' ' $ get (0,y)
        solve (pred -> x,         0) get = op ins ' ' (t V.! x) $ get (x,0)
        solve (pred -> x, pred -> y) get =
            let s' = s V.! x
                t' = t V.! y
            in if s' == t'
                then (Nothing:) <$> get (x, y)
                else minimumBy (compare `on` fst)
                     [ op del s' t' $ get (1+x, y) -- Delete
                     , op ins s' t' $ get (x, 1+y) -- Insert
                     , op sub s' t' $ get (x,y)    -- Substitute
                     ]
    in (reverse . catMaybes) <$> dp ix param solve (m * n)
