-- | Description: Framework for implementing dynamic programming algorithms.
--
--   This module provides a framework for implementing dynamic programming
--   algorithms by constructing a tableau. The caller provides implementations
--   of an isomorphism between the natural numbers and sub-problems, ordered
--   according to dependencies between problems, and a step function and the
--   code in this module does the rest.
module DynamicProgramming where

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
                get p = subs V.! index p
            in step (param i) get
        tableau = V.constructN n solve
    in V.last tableau
