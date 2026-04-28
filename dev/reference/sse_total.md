# Compute the total sum of squares

Compute the total sum of squares

## Usage

``` r
sse_total(object, ...)

# S3 method for class 'cluster_spec'
sse_total(object, ...)

# S3 method for class 'cluster_fit'
sse_total(object, new_data = NULL, dist_fun = NULL, ...)

# S3 method for class 'workflow'
sse_total(object, new_data = NULL, dist_fun = NULL, ...)

sse_total_vec(
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

  A function of the form `function(x, y)` that takes two matrices
  (centroids and observations) and returns a distance matrix. Defaults
  to
  [`philentropy::dist_many_many`](https://drostlab.github.io/philentropy/reference/dist_many_many.html)
  with Euclidean distance. See
  [`philentropy::getDistMethods()`](https://drostlab.github.io/philentropy/reference/getDistMethods.html)
  for a list of supported methods, and
  `vignette("tuning_and_metrics", package = "tidyclust")` for usage
  examples.

## Value

A tibble with 3 columns; `.metric`, `.estimator`, and `.estimate`.

## See also

Other cluster metric:
[`silhouette_avg()`](https://tidyclust.tidymodels.org/dev/reference/silhouette_avg.md),
[`sse_ratio()`](https://tidyclust.tidymodels.org/dev/reference/sse_ratio.md),
[`sse_within_total()`](https://tidyclust.tidymodels.org/dev/reference/sse_within_total.md)

## Examples

``` r
kmeans_spec <- k_means(num_clusters = 5) |>
  set_engine("stats")

kmeans_fit <- fit(kmeans_spec, ~., mtcars)

sse_total(kmeans_fit)
#> # A tibble: 1 × 3
#>   .metric   .estimator .estimate
#>   <chr>     <chr>          <dbl>
#> 1 sse_total standard     623387.

sse_total_vec(kmeans_fit)
#> [1] 623387.5
```
