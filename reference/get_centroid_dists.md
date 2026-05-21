# Computes distance from observations to centroids

Computes distance from observations to centroids

## Usage

``` r
get_centroid_dists(
  new_data,
  centroids,
  dist_fun = function(x, y) {
     philentropy::dist_many_many(x, y, method =
    "euclidean")
 }
)
```

## Arguments

- new_data:

  A data frame

- centroids:

  A data frame where each row is a centroid.

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
