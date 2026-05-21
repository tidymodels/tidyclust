# Get tune arguments for a cluster specification

Get tune arguments for a cluster specification

## Usage

``` r
# S3 method for class 'cluster_spec'
tune_args(object, full = FALSE, ...)
```

## Arguments

- object:

  A `model_spec`, `recipe`, `workflow`, or other object.

- ...:

  Other arguments passed to methods.

## Value

A tibble describing the tunable arguments in the cluster specification.
