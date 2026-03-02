# Prepares data and distance matrices for metric calculation

Prepares data and distance matrices for metric calculation

## Usage

``` r
prep_data_dist(
  object,
  new_data = NULL,
  dists = NULL,
  dist_fun = philentropy::distance
)
```

## Arguments

- object:

  A fitted
  [`cluster_spec`](https://tidyclust.tidymodels.org/dev/reference/cluster_spec.md)
  object.

- new_data:

  A dataset to calculate predictions on. If `NULL`, the trained cluster
  assignments from the fitted object are used.

- dists:

  A distance matrix for the data. If `NULL`, distance is computed on
  `new_data` using the
  [`stats::dist()`](https://rdrr.io/r/stats/dist.html) function.

- dist_fun:

  A custom distance functions.

## Value

A list
