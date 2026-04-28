# Turn a tidyclust model object into a tidy tibble

This method tidies the model in a tidyclust model object, if it exists.

## Usage

``` r
# S3 method for class 'cluster_fit'
tidy(x, ...)
```

## Arguments

- x:

  An object to be converted into a tidy
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).

- ...:

  Additional arguments to tidying method.

## Value

A tibble with one row per cluster. Columns depend on the underlying
engine but typically include `.cluster` and cluster-level summary
statistics such as centroid coordinates or cluster size.

## Examples

``` r
# tidy() support depends on the underlying engine. For the stats engine,
# broom must be installed.
if (FALSE) { # \dontrun{
kmeans_fit <- k_means(num_clusters = 3) |>
  set_engine("stats") |>
  fit(~., mtcars)

tidy(kmeans_fit)

hclust_fit <- hier_clust(num_clusters = 3) |>
  set_engine("stats") |>
  fit(~., mtcars)

tidy(hclust_fit)
} # }
```
