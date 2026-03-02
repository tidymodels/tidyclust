# Determine the minimum set of model fits

Determine the minimum set of model fits

## Usage

``` r
# S3 method for class 'cluster_spec'
min_grid(x, grid, ...)
```

## Arguments

- x:

  A cluster specification.

- grid:

  A tibble with tuning parameter combinations.

- ...:

  Not currently used.

## Value

A tibble with the minimum tuning parameters to fit and an additional
list column with the parameter combinations used for prediction.
