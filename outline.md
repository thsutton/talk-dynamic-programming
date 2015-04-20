Outline
=======

- Introduction

- Dynamic Programming

    - Overlapping sub-problems

        - From Wikipedia:

            > The problem can be broken down into sub-problems which are reused
            > several times.

    - Optimal substructure

        - From Wikipedia:

            > An optimal solution can be constructed efficiently from optimal
            > solutions of its sub-problems.

- Example: String edit distance problem

    - Wagner-Fischer algorithm

- Example: Shortest path problem

    - Dijkstra's algorithm

- Structuring a problem

- Implementation with Vector

    - Framework

        - Order the sub-problems appropriately.

        - Build a vector and fill it up with sub-problems.

        - With an appropriate ordering, `constructN` does exactly what we want.
          The constructing function takes `length v` as the index of the
          sub-problem to solve.

    - Wagner-Fischer algorithm

        - The ordering falls out of the table structure.

        - The vector is `n * m` long, each value depends only on cells before
          it in the ordering.

        - `length v` denotes a sub-problem: `length v / m` is the length of
          target string prefix and `length v % m` is the length of the source
          string prefix.

    - Dijkstra's algorithm

        - Each node in the graph becomes a sub-problem.

        - Use topological sort from the source node to assign an index each
          node. Ensures that for any sub-problem, the relevant sub-problems are
          are prior.

        - Again, `length v` denotes a sub-problem: `length v` is the ID of
          a node, find the minimum for the incoming edges.

