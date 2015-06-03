{-# LANGUAGE ViewPatterns #-}
module WF where

import           Control.Applicative
import           Control.Arrow
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

import           DynamicProgramming

-- * String Edit Distance
--
-- $ This code implementes the Wagner-Fischer algorithm (also know by other
--   names) to calculate the least-cost edit script which converts on string
--   into another.

-- | The parameters for a problem are the length of the target and source string
-- prefixes.
type Problem = (Int, Int)

type Solution = (Int, [Op])

-- | Edit scripts are sequences of 'Op's describing operations.
data Op
    = Del Int Char
    | Ins Int Char
    | Sub Int Char

del :: Int -> Char -> Char -> Op
del i c _ = Del i c

ins :: Int -> Char -> Char -> Op
ins i _ = Ins i

sub :: Int -> Char -> Char -> Op
sub i _ = Sub i

-- | Used to calculate the position to be affected by an operation.
offset :: Op -> Int
offset (Del _ _) = 0
offset (Ins _ _) = 1
offset (Sub _ _) = 1

-- | We'll use this to append a particular operation function above to our
-- solutions above.
op  :: (Int -> Char -> Char -> Op)
    -> Char -> Char
    -> (Int, [Maybe Op])
    -> (Int, [Maybe Op])
op f s' t' p =
    let c = f (position . snd $ p) s' t' 
    in succ *** (Just c :) $ p
  where
    position :: [Maybe Op] -> Int
    position = sum . fmap (maybe 1 offset)

-- | Find the `Index` for a string edit distance sub-problem.
editIx
    :: Size    -- ^ Width of the tableau.
    -> Problem -- ^ Length of prefix in t and s strings.
    -> Index
editIx n (x,y) = (x * n) + y

-- | Find the string edit distance sub-problem located at an `Index`.
editParam
    :: Size  -- ^ Width of the tableau.
    -> Index -- ^ Position in tableau.
    -> Problem
editParam n i = i `quotRem` n

-- | Determine the cost and script for an edit-distance problem using the
-- Wagner-Fischer algorithm.
editDistance :: Vector Char -> Vector Char -> Solution
editDistance s t =
    let m = V.length s
        n = V.length t
        ix    = editIx n
        param = editParam n
        solve (        0,         0) _   = (0, mempty)
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
    in (reverse . catMaybes) <$>
        dp ix param solve (m * n)
