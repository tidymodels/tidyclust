# Simple Wrapper around klaR kmeans

This wrapper runs
[`klaR::kmodes()`](https://rdrr.io/pkg/klaR/man/kmodes.html) and
reorders the clusters.

## Usage

``` r
.k_means_fit_klaR(data, modes, ...)
```

## Arguments

- data:

  A matrix or data frame of categorical data. Objects have to be in
  rows, variables in columns.

- modes:

  Either the number of modes or a set of initial (distinct) cluster
  modes. If a number, a random set of (distinct) rows in `data` is
  chosen as the initial modes.

- ...:

  Other arguments passed to
  [`klaR::kmodes()`](https://rdrr.io/pkg/klaR/man/kmodes.html)

## Value

Result from [`klaR::kmodes()`](https://rdrr.io/pkg/klaR/man/kmodes.html)
