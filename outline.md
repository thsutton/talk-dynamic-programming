% Dynamic Programming in Haskell
% Thomas Sutton, Anchor
% 2015-05-22

# Introduction

## Introduction

This is a talk in two parts:

1. First I'll introduce [dynamic programming][wp:dp] and describe several
example problems.

2. Second I'll described a framework for addressing these problems in
[Haskell][haskell] using the [vector][hs:vec] library.

# Dynamic Programming

## Dynamic programming

[*Dynamic programming*][wp:dp] is an approach to solving problems which exhibit
two properties:

- [*Overlapping sub-problems*][wp:osp] - problems are divided into
**sub-problems** which **are used several times** in the calculation of
a solution to the overall problem.

- [*Optimal substructure*][wp:oss] - an **optimal solution** can be found
efficiently given optimal **solutions to its sub-problems**.

In practice this means:

- Given optimal solutions to all immediate sub-problems you can efficiently
construct an optimal solution to your problem;

- It is worthwhile to compute solutions to all sub-problems, working bottom to
assemble them into a solution to the overall problem.

## Dynamic programming

Solving a single "problem" then requires:

1. Finding all candidate sub-problem/s;

2. Selecting the optimal sub-problem/s solutions; and

3. Combining the selected sub-problem/s solutions.

This is combined with an ordering on sub-problems which prioritises solving
"smaller" problems over "larger" ones; and a driver to run the procedure and
build a *tableau* of solutions according to the order.

## Other approaches

Dynamic programming can be contrasted with other approaches:

- *Divide and conquer* algorithms have sub-problems which do not overlap;
solutions are not re-used.

- *Greedy* algorithms work top-down selecting *locally* optimal sub-problems.

# Example problems

## Examples

There are many dynamic programming problems, I'll be using the following as
examples:

- *Production line scheduling* - given a factory with multiple production
lines, find the cheapest schedule for a unit of work to move through the lines.

- *Matrix-chain multiplication* - given a sequence of compatible matrices, find
the optimal order in which to associate the multiplications.

- *String edit distance* - given two strings, find the lowest-cost sequence of
operations to change the first into the second.

## Production Line Scheduling

## Matrix-chain multiplication

Matrix multiplication is a pretty big deal. Assuming you have two matrices with
dimensions $A_{1} : m \times n$ and $A_{2} : n \times o$ then multiplying them
will take on the order of $O(m \times n \times o)$ scalar operations.

If you have a chain of $n > 1$ matrices to multiply together, the order in
which you perform the multiplications can make a very large different to the
number of operations performed.

## Example: Multiply three matrices

\begin{align*}
A_1 &: 10 \times 100    \\
A_2 &: 100 \times 5 \\
A_3 &: 5 \times 50
\end{align*}

There are two ways we can evaluate the change $A_1 A_2 A_3$: $(A_1 A_2) A_3$ or

\begin{align*}
(A_1 A_2) A_3 &= (10 \times 100 \times 5) + (10 \times 5 \times 50) &&= 7500 \\
A_1 (A_2 A_3) &= (10 \times 100 \times 50) + (100 \times 5 \times 50) &&= 75000
\end{align*}

We've only had to make one choice and we've already, potentially, done an order
of magnitude too much work!

## Example: String edit distance

Given two strings, find the optimal cost (and/or the sequence of operations) to
transform the first string into the latter.

We aren't committed to any particular set of operations but we'll use:

    - Insert: $cost(cat \rightarrow chat) = 1$

    - Delete: $cost(cat \rightarrow ca) = 1$

    - Substitute: $cost(cat \rightarrow sat) = 1$

## Wagner-Fischer algorithm

[Wagner-Fischer algorithm][wp:wfa] uses dynamic programming to find optimal
solution for edit distance problems.



## Structure of problems

LOL WUT

# Implementation in Haskell

## Overview

The key observation is that all these algorithms start with an empty tableau
and gradually fill it in as they solve progressively larger sub-problems.

If we can find an appropriate ordering on sub-problems we can make use of some
of the construction functions provided by libraries like `vector` to implement
these algorithms.

## Framework

1. Impose a total order on sub-problems such that "small" problems come before
larger ones.

2. Implement an isomorphism between the order (i.e. `Int`) and the parameters
which characterise a sub-problem.

3. Implement a function which, given a `Vector` of solved "prior" problems,
generates an optimal solution for the current problem.

4. Glue these together by using `Data.Vector.constructN`.

## Implementation

````{.haskell}
type Size = Int
type Index = Size

dp :: (prob -> Index)
   -> (Index -> prob)
   -> (prob -> (prob -> sol) -> sol)
   -> Size
   -> sol
````

````{.haskell}
dp index param step n =
    let solve subs =
            let p = param (V.length subs)
                get p = subs V.! (index p)
            in solve p get
        tableau = C.constructN n solve
    in V.last tableau
````

## Wagner-Fischer algorithm

- The ordering falls out of the table structure.

- The vector is `n * m` long, each value depends only on cells before it in the
ordering.

- `length v` denotes a sub-problem: `length v / m` is the length of target
string prefix and `length v % m` is the length of the source string prefix.

## Matrix-chain multiplication

# Conclusion

## Conclusions

- Dynamic programming is a great and fits naturally into standard libraries in
the Haskell ecosystem.

- The mutation used in the descriptions of many algorithms is often incidental;
you can probably find a way to remove it or hide it behind an API.

- Finding a suitable isomorphism $Index \leftrightarrow problem$ which orders
sub-problems appropriately is the key; if you care about complexity analysis of
the whole algorithm this should probably be $O(1)$.

[haskell]: https://www.haskell.org/
[hs:vec]: https://hackage.haskell.org/package/vector
[wp:dp]: https://en.wikipedia.org/wiki/Dynamic_programming
[wp:osp]: https://en.wikipedia.org/wiki/Overlapping_subproblems
[wp:oss]: https://en.wikipedia.org/wiki/Optimal_substructure
[wp:wfa]: https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm
