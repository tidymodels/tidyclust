# The agglomeration Linkage method

The agglomeration Linkage method

## Usage

``` r
linkage_method(values = values_linkage_method)

values_linkage_method
```

## Format

An object of class `character` of length 8.

## Arguments

- values:

  A character string of possible values. See `linkage_methods` in
  examples below.

## Value

A `dials` parameter object for use with
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
and related functions.

## Details

This parameter is used in `tidyclust` models for
[`hier_clust()`](https://tidyclust.tidymodels.org/dev/reference/hier_clust.md).

## Examples

``` r
values_linkage_method
#> [1] "ward.D"   "ward.D2"  "single"   "complete" "average"  "mcquitty"
#> [7] "median"   "centroid"
linkage_method()
#> Linkage Method (qualitative)
#> 8 possible values include:
#> 'ward.D', 'ward.D2', 'single', 'complete', 'average', 'mcquitty',
#> 'median', and 'centroid'
```
