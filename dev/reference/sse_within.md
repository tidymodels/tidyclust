# Calculates Sum of Squared Error in each cluster

Calculates Sum of Squared Error in each cluster

## Usage

``` r
sse_within(
  object,
  new_data = NULL,
  dist_fun = function(x, y) {
     philentropy::dist_many_many(x, y, method =
    "euclidean")
 }
)
```

## Arguments

- object:

  A fitted kmeans tidyclust model

- new_data:

  A dataset to predict on. If `NULL`, uses trained clustering.

- dist_fun:

  A function for calculating distances to centroids. Defaults to
  Euclidean distance on processed data.

## Value

A tibble with two columns, the cluster name and the SSE within that
cluster.

## Details

[`sse_within_total()`](https://tidyclust.tidymodels.org/dev/reference/sse_within_total.md)
is the corresponding cluster metric function that returns the sum of the
values given by `sse_within()`.

## Examples

``` r
kmeans_spec <- k_means(num_clusters = 5) |>
  set_engine("stats")

kmeans_fit <- fit(kmeans_spec, ~., mtcars)

sse_within(kmeans_fit)
#> # A tibble: 5 × 3
#>   .cluster     wss n_members
#>   <fct>      <dbl>     <int>
#> 1 Cluster_1  7256.         6
#> 2 Cluster_2  3617.         7
#> 3 Cluster_3  6356.         6
#> 4 Cluster_4 46659.         9
#> 5 Cluster_5   208.         4
```
