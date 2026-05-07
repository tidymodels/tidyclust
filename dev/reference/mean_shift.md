# Mean Shift Clustering

`mean_shift()` defines a model that fits clusters by iteratively
shifting observations toward regions of high density, with the number of
clusters determined automatically from the data.

There are different implementations for this model, and the
implementation is chosen by setting the model engine. The
engine-specific pages for this model are listed below.

- [LPCM](https://tidyclust.tidymodels.org/dev/reference/details_mean_shift_LPCM.md)

## Usage

``` r
mean_shift(mode = "partition", engine = "LPCM", bandwidth = NULL)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is `"partition"`.

- engine:

  A single character string specifying what computational engine to use
  for fitting. The default engine for this model is `"LPCM"`.

- bandwidth:

  Positive double, kernel bandwidth controlling the size of the
  neighborhood used to compute the density estimate (required).

## Value

A `mean_shift` cluster specification.

## Details

### What does it mean to predict?

To predict the cluster assignment for a new observation, the mean shift
procedure is run from the new point until it converges to a mode. The
observation is then assigned to the cluster of the nearest discovered
training mode.

## Examples

``` r
# Show all engines
modelenv::get_from_env("mean_shift")
#> # A tibble: 1 × 2
#>   engine mode     
#>   <chr>  <chr>    
#> 1 LPCM   partition

mean_shift()
#> Mean Shift Clustering Specification (partition)
#> 
#> Computational engine: LPCM 
#> 
```
