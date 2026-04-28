# Get required packages for a cluster object

Get required packages for a cluster object

## Usage

``` r
# S3 method for class 'cluster_spec'
required_pkgs(x, infra = TRUE, ...)

# S3 method for class 'cluster_fit'
required_pkgs(x, infra = TRUE, ...)
```

## Arguments

- x:

  A
  [`cluster_spec`](https://tidyclust.tidymodels.org/dev/reference/cluster_spec.md)
  or
  [`cluster_fit`](https://tidyclust.tidymodels.org/dev/reference/cluster_fit.md)
  object.

- infra:

  A logical. Should tidyclust itself be included in the result?

- ...:

  Currently unused.

## Value

A character vector of required package names.
