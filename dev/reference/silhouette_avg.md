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

  A function of the form `function(x)` that takes a data frame or matrix
  and returns a `dist` object. Defaults to
  [`philentropy::distance`](https://drostlab.github.io/philentropy/reference/distance.html)
  with Euclidean distance. See
  [`philentropy::getDistMethods()`](https://drostlab.github.io/philentropy/reference/getDistMethods.html)
  for a list of supported methods, and
  `vignette("tuning_and_metrics", package = "tidyclust")` for usage
  examples.

## Value

A double; the average silhouette.

## Details

Not to be confused with
[`silhouette()`](https://tidyclust.tidymodels.org/dev/reference/silhouette.md)
that returns a tibble with silhouette for each observation. The
silhouette coefficient ranges from -1 to 1, where values close to 1
indicate well-separated clusters. This metric has
`direction = "maximize"`, so
[`tune::select_best()`](https://tune.tidymodels.org/reference/show_best.html)
and
[`tune::show_best()`](https://tune.tidymodels.org/reference/show_best.html)
will return models with the highest silhouette values.

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
