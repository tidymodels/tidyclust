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

All functions must be cluster metrics. To include a metric that wraps a
built-in metric with custom arguments, such as
[`silhouette_avg()`](https://tidyclust.tidymodels.org/dev/reference/silhouette_avg.md)
with a non-default `dist_fun`, first wrap it with
[`new_cluster_metric()`](https://tidyclust.tidymodels.org/dev/reference/new_cluster_metric.md)
so that it carries the `cluster_metric` class. See the examples in
[`new_cluster_metric()`](https://tidyclust.tidymodels.org/dev/reference/new_cluster_metric.md).

## See also

[`new_cluster_metric()`](https://tidyclust.tidymodels.org/dev/reference/new_cluster_metric.md)
