# Gaussian mixture covariance structure parameters

Logical flags controlling the covariance structure of cluster Gaussians
fit by
[`tidyclust::gm_clust()`](https://tidyclust.tidymodels.org/reference/gm_clust.md)
with the `mclust` engine. See
[`gm_clust()`](https://tidyclust.tidymodels.org/reference/gm_clust.md)
for descriptions.

## Usage

``` r
circular(values = c(TRUE, FALSE))

zero_covariance(values = c(TRUE, FALSE))

shared_orientation(values = c(TRUE, FALSE))

shared_shape(values = c(TRUE, FALSE))

shared_size(values = c(TRUE, FALSE))
```

## Arguments

- values:

  A vector of possible values (`c(TRUE, FALSE)` by default).

## Value

A `dials` parameter object for use with
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
and related functions.

## Examples

``` r
circular()
#> Circular MVG (qualitative)
#> 2 possible values include:
#> TRUE and FALSE
zero_covariance()
#> Zero Covariance (qualitative)
#> 2 possible values include:
#> TRUE and FALSE
shared_orientation()
#> Shared Orientation (qualitative)
#> 2 possible values include:
#> TRUE and FALSE
shared_shape()
#> Shared Shape (qualitative)
#> 2 possible values include:
#> TRUE and FALSE
shared_size()
#> Shared Size (qualitative)
#> 2 possible values include:
#> TRUE and FALSE
```
