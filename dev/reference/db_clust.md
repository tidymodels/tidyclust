# Density-Based Spatial Clustering of Applications with Noise (DBSCAN)

`db_clust` defines a model that fits clusters based on areas with
observations that are densely packed together using the DBSCAN algorithm

There are multiple implementations for this model, and the
implementation is chosen by setting the model engine. The
engine-specific pages for this model are listed below.

- [dbscan](https://tidyclust.tidymodels.org/dev/reference/details_db_clust_dbscan.md)

## Usage

``` r
db_clust(
  mode = "partition",
  engine = "dbscan",
  radius = NULL,
  min_points = NULL
)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is `"partition"`.

- engine:

  A single character string specifying what computational engine to use
  for fitting. The engine for this model is `"dbscan"`.

- radius:

  Positive double, Radius drawn around points to determine core-points
  and cluster assignments (required).

- min_points:

  Positive integer, Minimum number of connected points required to form
  a core-point, including the point itself (required).

## Value

A `db_clust` cluster specification.

## Details

### What does it mean to predict?

To predict the cluster assignment for a new observation, we determine if
a point is within the radius of a core point. If so, we predict the same
cluster as the core point. If not, we predict the observation to be an
outlier.

## Examples

``` r
# Show all engines
modelenv::get_from_env("db_clust")
#> # A tibble: 1 × 2
#>   engine mode     
#>   <chr>  <chr>    
#> 1 dbscan partition

db_clust()
#> DBSCAN Clustering Specification (partition)
#> 
#> Computational engine: dbscan 
#> 
```
