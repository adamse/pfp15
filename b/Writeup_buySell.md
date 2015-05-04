# Work and span of buySell algorithm

Assuming input of length $N$.

Our algorithm has 3 steps:

* `buildMatrix`: builds a matrix of all possible buy and sell dates

    - work: $N^2$
    - span: 1, there are no dependencies between elements of the matrix

* `getSell`: computes the best sell date for each buy date

    A `fold` has work $N$ and span $\log N$. We have $N$ folds folding
    over $N$ elements:

    - work: $N^2$
    - span: $\log N$, it is possible to run all $N$ folds in parallel.

* `getBuy`: computes the best buy date

    - work: $N$
    - span: $\log N$


In total we have

- work: $2N^2+N$ and
- span: $2\log N + 1$
