# Simple Wrapper around clustMixType kmeans

This wrapper runs
[`clustMixType::kproto()`](https://rdrr.io/pkg/clustMixType/man/kproto.html)
and reorders the clusters.

## Usage

``` r
.k_means_fit_clustMixType(x, k, ...)
```

## Arguments

- x:

  Data frame with both numerics and factors (also ordered factors are
  possible).

- k:

  Either the number of clusters, a vector specifying indices of initial
  prototypes, or a data frame of prototypes of the same columns as `x`.

- ...:

  Other arguments passed to
  [`clustMixType::kproto()`](https://rdrr.io/pkg/clustMixType/man/kproto.html)

## Value

Result from
[`clustMixType::kproto()`](https://rdrr.io/pkg/clustMixType/man/kproto.html)
