# Radius

The radius used by density-based clustering to determine core points and
cluster assignments. Used in
[`tidyclust::db_clust()`](https://tidyclust.tidymodels.org/dev/reference/db_clust.md)
with the `dbscan` engine.

## Usage

``` r
radius(range = c(0, dials::unknown()), trans = NULL)
```

## Arguments

- range:

  A two-element vector holding the *defaults* for the smallest and
  largest possible values, respectively. If a transformation is
  specified, these values should be in the *transformed units*.

- trans:

  A `trans` object from the `scales` package, such as
  [`scales::transform_log10()`](https://scales.r-lib.org/reference/transform_log.html)
  or
  [`scales::transform_reciprocal()`](https://scales.r-lib.org/reference/transform_reciprocal.html).
  If not provided, the default is used which matches the units used in
  `range`. If no transformation, `NULL`.

## Value

A `dials` parameter object for use with
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
and related functions.

## Examples

``` r
radius()
#> Radius (quantitative)
#> Range: (0, ?]
```
