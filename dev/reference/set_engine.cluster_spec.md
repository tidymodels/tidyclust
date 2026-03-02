# Change engine of a cluster specification

Change engine of a cluster specification

## Usage

``` r
# S3 method for class 'cluster_spec'
set_engine(object, engine, ...)
```

## Arguments

- object:

  A [model
  specification](https://parsnip.tidymodels.org/reference/model_spec.html).

- engine:

  A character string for the software that should be used to fit the
  model. This is highly dependent on the type of model (e.g. linear
  regression, random forest, etc.).

- ...:

  Any optional arguments associated with the chosen computational
  engine. These are captured as quosures and can be tuned with
  [`tune()`](https://hardhat.tidymodels.org/reference/tune.html).

## Value

An updated
[`cluster_spec`](https://tidyclust.tidymodels.org/dev/reference/cluster_spec.md)
object.
