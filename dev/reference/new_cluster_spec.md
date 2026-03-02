# Functions required for tidyclust-adjacent packages

These functions are helpful when creating new packages that will
register new cluster specifications.

## Usage

``` r
new_cluster_spec(cls, args, eng_args, mode, method, engine)
```

## Value

A
[`cluster_spec`](https://tidyclust.tidymodels.org/dev/reference/cluster_spec.md)
object made to work with tidyclust.
