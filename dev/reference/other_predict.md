# Other predict methods.

These are internal functions not meant to be directly called by the
user.

## Usage

``` r
predict_cluster(object, ...)

# S3 method for class 'cluster_fit'
predict_cluster(object, new_data, ...)
```

## Arguments

- object:

  An object of class
  [`cluster_fit`](https://tidyclust.tidymodels.org/dev/reference/cluster_fit.md).

- ...:

  Optional arguments passed to the underlying predict function. Use
  `prefix` to change the prefix in the cluster factor levels (default:
  `"Cluster_"`). Use `labels` to supply a character vector of cluster
  labels, which overrides `prefix`.

- new_data:

  A rectangular data object, such as a data frame.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).
