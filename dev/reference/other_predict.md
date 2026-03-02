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

  Arguments to the underlying model's prediction function cannot be
  passed here (see `opts`).

- new_data:

  A rectangular data object, such as a data frame.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).
