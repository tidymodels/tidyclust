# Construct a single row summary "glance" of a model, fit, or other object

This method glances the model in a tidyclust model object, if it exists.

## Usage

``` r
# S3 method for class 'cluster_fit'
glance(x, ...)
```

## Arguments

- x:

  model or other R object to convert to single-row data frame

- ...:

  other arguments passed to methods

## Value

A one-row tibble with model-level summary statistics such as total
within-cluster sum of squares, between-cluster sum of squares, and
number of iterations. Support depends on the underlying engine.

## Examples

``` r
# glance() support depends on the underlying engine.
if (FALSE) { # \dontrun{
kmeans_fit <- k_means(num_clusters = 3) |>
  set_engine("stats") |>
  fit(~., mtcars)

glance(kmeans_fit)
} # }
```
