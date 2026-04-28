# Get tunable parameters for a cluster specification

Get tunable parameters for a cluster specification

## Usage

``` r
# S3 method for class 'cluster_spec'
tunable(x, ...)

# S3 method for class 'k_means'
tunable(x, ...)

# S3 method for class 'db_clust'
tunable(x, ...)

# S3 method for class 'gm_clust'
tunable(x, ...)
```

## Arguments

- x:

  An object, such as a recipe, recipe step, workflow, or model
  specification.

- ...:

  Other arguments passed to methods

## Value

A tibble with columns `name`, `call_info`, `source`, `component`, and
`component_id` describing each tunable parameter.
