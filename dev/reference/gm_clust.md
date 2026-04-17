# Gaussian Mixture Models (GMM)

`gm_clust` defines a model that fits clusters based on fitting a
specified number of multivariate Gaussian distributions (MVG) to the
data.

There are multiple implementations for this model, and the
implementation is chosen by setting the model engine. The
engine-specific pages for this model are listed below.

- [mclust](https://tidyclust.tidymodels.org/dev/reference/details_gm_clust_mclust.md)

## Usage

``` r
gm_clust(
  mode = "partition",
  engine = "mclust",
  num_clusters = NULL,
  circular = TRUE,
  shared_size = TRUE,
  zero_covariance = TRUE,
  shared_orientation = TRUE,
  shared_shape = TRUE
)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "partition".

- engine:

  A single character string specifying what computational engine to use
  for fitting. The engine for this model is `"mclust"`.

- num_clusters:

  Positive integer, number of clusters in model (required).

- circular:

  Boolean, whether or not to fit circular MVG distributions for each
  cluster. Default `TRUE`.

- shared_size:

  Boolean, whether each cluster MVG should have the same size/volume.
  Default `TRUE`.

- zero_covariance:

  Boolean, whether or not to assign covariances of 0 for each MVG.
  Default `TRUE`.

- shared_orientation:

  Boolean, whether each cluster MVG should have the same orientation.
  Default `TRUE`.

- shared_shape:

  Boolean, whether each cluster MVG should have the same shape. Default
  `TRUE`.

## Value

A `gm_clust` cluster specification.

## Details

### What does it mean to predict?

To predict the cluster assignment for a new observation, we determine
which cluster a point has the highest probability of belonging to.

## Examples

``` r
# Show all engines
modelenv::get_from_env("gm_clust")
#> # A tibble: 1 × 2
#>   engine mode     
#>   <chr>  <chr>    
#> 1 mclust partition

gm_clust()
#> GMM Clustering Specification (partition)
#> 
#> Main Arguments:
#>   circular = TRUE
#>   zero_covariance = TRUE
#>   shared_orientation = TRUE
#>   shared_shape = TRUE
#>   shared_size = TRUE
#> 
#> Computational engine: mclust 
#> 
```
