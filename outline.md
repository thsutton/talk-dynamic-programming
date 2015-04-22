% Dynamic Programming in Hasekll
% Thomas Sutton, Anchor
% 2015-05-22

# Introduction

This is a talk in two parts:

1. First I'll introduce [dynamic programming][wp:dp] and review several example
problems.

2. Second I'll described a framework for addressing these problems in
[Haskell][1] using the [vector][hs:vec] library.

[1]: https://www.haskell.org/
[hs:vec]: https://hackage.haskell.org/package/vector

# Dynamic Programming

- Overlapping sub-problems

    > The problem can be broken down into sub-problems which are reused
    > several times.
    >
    > -- [Wikipedia][wp:dp]

- Optimal substructure

    > An optimal solution can be constructed efficiently from optimal
    > solutions of its sub-problems.
    >
    > -- [Wikipedia][wp:dp]

[wp:dp]: https://en.wikipedia.org/wiki/Dynamic_programming

# Example: String edit distance problem

- We have two strings and want to find the difference between them. We aren't
committed to any particular set of operations but we'll use:

    - Insert $cost(cat \rightarrow chat) = 1$

    - Delete $cost(cat \rightarrow ca) = 1$

    - Substitute $cost(cat \rightarrow sat) = 1$

## The Wagner-Fischer algorithm

[Wagner-Fischer algorithm][wp:wfa] uses dynamic programming to find optimal
solution for edit distance problems.

[wp:wfa]: https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm

# The tableau

# Example: Matrix-chain multiplication

Matrix multiplication is a pretty big deal. Assuming you have two matrices with
dimensions $A_{1} : m \times n$ and $A_{2} : n \times o$ then multiplying them
will take on the order of $O(m \times n \times o)$ scalar operations.

If you have a chain of $n > 1$ matrices to multiply together, the order in
which you perform the multiplications can make a very large different to the
number of operations performed.

# Example: Multiply three matrices

$$A_1 : 10 \times 100$$
$$A_2 : 100 \times 5$$
$$A_3 : 5 \times 50$$

There are two ways we can evaluate the change $A_1 A_2 A_3$: $(A_1 A_2) A_3$ or

$$(A_1 A_2) A_3 = (10 \times 100 \times 5) + (10 \times 5 \times 50) = 7500$$

$$A_1 (A_2 A_3) = (100 \times 5 \times 50) + (10 \times 100 \times 50) = 75000$$

We've only had to make one choice and we've already, potentially, done an order
of magnitude too much work!

# Structuring a problem

# Implementation with Vector

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

## Wagner-Fischer algorithm

- The ordering falls out of the table structure.

- The vector is `n * m` long, each value depends only on cells before it in the
ordering.

- `length v` denotes a sub-problem: `length v / m` is the length of target
string prefix and `length v % m` is the length of the source string prefix.

## Dijkstra's algorithm

- Each node in the graph becomes a sub-problem.

- Use topological sort from the source node to assign an index each node.
Ensures that for any sub-problem, the relevant sub-problems are are prior.

- Again, `length v` denotes a sub-problem: `length v` is the ID of a node, find
the minimum for the incoming edges.

- Unfortunately we're restricting ourselves here (and also adding complexity).
