# Simple Wrapper around stats kmeans

This wrapper runs
[`stats::kmeans()`](https://rdrr.io/r/stats/kmeans.html) and adds a
check that `centers` is specified. And reorders the clusters.

## Usage

``` r
.k_means_fit_stats(data, centers = NULL, ...)
```

## Arguments

- centers:

  either the number of clusters, say \\k\\, or a set of initial
  (distinct) cluster centres. If a number, a random set of (distinct)
  rows in `x` is chosen as the initial centres.

- ...:

  Other arguments passed to
  [`stats::kmeans()`](https://rdrr.io/r/stats/kmeans.html)

## Value

Result from [`stats::kmeans()`](https://rdrr.io/r/stats/kmeans.html)
