# Construct a new clustering metric function

This function provides a convenient wrapper to create the one type of
metric function used in tidyclust: clustering metrics. It adds a
metric-specific class to `fn`. These features are used by
[`cluster_metric_set()`](https://tidyclust.tidymodels.org/reference/cluster_metric_set.md)
and by
[`tune_cluster()`](https://tidyclust.tidymodels.org/reference/tune_cluster.md)
when tuning.

Use `new_cluster_metric()` when you want to author your own clustering
metric, for example to call
[`silhouette_avg()`](https://tidyclust.tidymodels.org/reference/silhouette_avg.md)
with a non-default `dist_fun`. A plain function cannot be passed to
[`cluster_metric_set()`](https://tidyclust.tidymodels.org/reference/cluster_metric_set.md)
directly; it must first be wrapped with `new_cluster_metric()` so that
it carries the `cluster_metric` class.

## Usage

``` r
new_cluster_metric(fn, direction)
```

## Arguments

- fn:

  A function. It should take `object` and `new_data` as its first two
  arguments and return a single-row tibble, as produced by the built-in
  metrics such as
  [`silhouette_avg()`](https://tidyclust.tidymodels.org/reference/silhouette_avg.md).

- direction:

  A string. One of:

  - `"maximize"`

  - `"minimize"`

  - `"zero"`

## Value

A `cluster_metric` object.

## See also

[`cluster_metric_set()`](https://tidyclust.tidymodels.org/reference/cluster_metric_set.md)

## Examples

``` r
# Author a custom metric that uses a non-default distance function. Here we
# use the average silhouette with Chebyshev (L-infinity) distance.
linf_dist <- function(x) philentropy::distance(x, method = "chebyshev")

linf_silhouette_avg <- new_cluster_metric(
  function(object, new_data = NULL, ...) {
    silhouette_avg(object, new_data = new_data, dist_fun = linf_dist, ...)
  },
  direction = "maximize"
)

# The custom metric can now be combined with others in a metric set.
cluster_metric_set(linf_silhouette_avg, sse_ratio)
#> # A tibble: 2 × 3
#>   metric              class          direction
#>   <chr>               <chr>          <chr>    
#> 1 linf_silhouette_avg cluster_metric maximize 
#> 2 sse_ratio           cluster_metric zero     
```
