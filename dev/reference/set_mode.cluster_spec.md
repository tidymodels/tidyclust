# Change mode of a cluster specification

Change mode of a cluster specification

## Usage

``` r
# S3 method for class 'cluster_spec'
set_mode(object, mode, ...)
```

## Arguments

- object:

  A [model
  specification](https://parsnip.tidymodels.org/reference/model_spec.html).

- mode:

  A character string for the model type (e.g. "classification" or
  "regression")

- ...:

  One or more named model arguments.

## Value

An updated
[`cluster_spec`](https://tidyclust.tidymodels.org/dev/reference/cluster_spec.md)
object.
