# Compute the ratio of the WSS to the total SSE

Compute the ratio of the WSS to the total SSE

## Usage

``` r
sse_ratio(object, ...)

# S3 method for class 'cluster_spec'
sse_ratio(object, ...)

# S3 method for class 'cluster_fit'
sse_ratio(object, new_data = NULL, dist_fun = NULL, ...)

# S3 method for class 'workflow'
sse_ratio(object, new_data = NULL, dist_fun = NULL, ...)

sse_ratio_vec(
  object,
  new_data = NULL,
  dist_fun = function(x, y) {
     philentropy::dist_many_many(x, y, method =
    "euclidean")
 },
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

- dist_fun:

  A function for calculating distances to centroids. Defaults to
  Euclidean distance on processed data.

## Value

A tibble with 3 columns; `.metric`, `.estimator`, and `.estimate`.

## See also

Other cluster metric:
[`silhouette_avg()`](https://tidyclust.tidymodels.org/dev/reference/silhouette_avg.md),
[`sse_total()`](https://tidyclust.tidymodels.org/dev/reference/sse_total.md),
[`sse_within_total()`](https://tidyclust.tidymodels.org/dev/reference/sse_within_total.md)

## Examples

``` r
kmeans_spec <- k_means(num_clusters = 5) |>
  set_engine("stats")

kmeans_fit <- fit(kmeans_spec, ~., mtcars)

sse_ratio(kmeans_fit)
#> # A tibble: 1 × 3
#>   .metric   .estimator .estimate
#>   <chr>     <chr>          <dbl>
#> 1 sse_ratio standard      0.0687

sse_ratio_vec(kmeans_fit)
#> [1] 0.06873637
```
