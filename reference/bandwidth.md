# Bandwidth

The kernel bandwidth used by mean shift to estimate the local density
gradient. Smaller values yield more clusters, while larger values merge
them.

## Usage

``` r
bandwidth(range = c(0.01, 1), trans = NULL)
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

## Details

Used in
[`tidyclust::mean_shift()`](https://tidyclust.tidymodels.org/reference/mean_shift.md)
models. The scale on which the bandwidth is interpreted depends on the
engine, since some engines rescale predictors internally before applying
the kernel.

## Examples

``` r
bandwidth()
#> Bandwidth (quantitative)
#> Range: (0.01, 1]
```
