# Combine metric functions

`cluster_metric_set()` allows you to combine multiple metric functions
together into a new function that calculates all of them at once.

## Usage

``` r
cluster_metric_set(...)
```

## Arguments

- ...:

  The bare names of the functions to be included in the metric set.
  These functions must be cluster metrics such as
  [`sse_total()`](https://tidyclust.tidymodels.org/dev/reference/sse_total.md),
  [`sse_ratio()`](https://tidyclust.tidymodels.org/dev/reference/sse_ratio.md),
  or
  [`silhouette_avg()`](https://tidyclust.tidymodels.org/dev/reference/silhouette_avg.md).

## Value

A `cluster_metric_set()` object, combining the use of all input metrics.

## Details

All functions must be:

- Only cluster metrics
