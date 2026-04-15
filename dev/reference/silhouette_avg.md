# Measures average silhouette across all observations

Measures average silhouette across all observations

## Usage

``` r
silhouette_avg(object, ...)

# S3 method for class 'cluster_spec'
silhouette_avg(object, ...)

# S3 method for class 'cluster_fit'
silhouette_avg(object, new_data = NULL, dists = NULL, dist_fun = NULL, ...)

# S3 method for class 'workflow'
silhouette_avg(object, new_data = NULL, dists = NULL, dist_fun = NULL, ...)

silhouette_avg_vec(
  object,
  new_data = NULL,
  dists = NULL,
  dist_fun = philentropy::distance,
  ...
)
```

## Arguments

- object:

  A fitted kmeans tidyclust model

- ...:

  Other arguments passed to methods.

- new_data:

  A dataset to predict on. If `NULL`, uses trained clustering.

- dists:

  A distance matrix. Used if `new_data` is `NULL`.

- dist_fun:

  A function for calculating distances between observations. Defaults to
  Euclidean distance on processed data.

## Value

A double; the average silhouette.

## Details

Not to be confused with
[`silhouette()`](https://tidyclust.tidymodels.org/dev/reference/silhouette.md)
that returns a tibble with silhouette for each observation.

## See also

Other cluster metric:
[`sse_ratio()`](https://tidyclust.tidymodels.org/dev/reference/sse_ratio.md),
[`sse_total()`](https://tidyclust.tidymodels.org/dev/reference/sse_total.md),
[`sse_within_total()`](https://tidyclust.tidymodels.org/dev/reference/sse_within_total.md)

## Examples

``` r
kmeans_spec <- k_means(num_clusters = 5) |>
  set_engine("stats")

kmeans_fit <- fit(kmeans_spec, ~., mtcars)

dists <- mtcars |>
  as.matrix() |>
  dist()

silhouette_avg(kmeans_fit, dists = dists)
#> # A tibble: 1 × 3
#>   .metric        .estimator .estimate
#>   <chr>          <chr>          <dbl>
#> 1 silhouette_avg standard       0.345

silhouette_avg_vec(kmeans_fit, dists = dists)
#> [1] 0.3450963
```
