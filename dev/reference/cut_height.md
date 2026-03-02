# Cut Height

Used in most
[`tidyclust::hier_clust()`](https://tidyclust.tidymodels.org/dev/reference/hier_clust.md)
models.

## Usage

``` r
cut_height(range = c(0, dials::unknown()), trans = NULL)
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

## Examples

``` r
cut_height()
#> Cut Height (quantitative)
#> Range: [0, ?]
```
