# K-Means

`k_means()` defines a model that fits clusters based on distances to a
number of centers. This definition doesn't just include K-means, but
includes models like K-prototypes.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model engine. The engine-specific pages for
this model are listed below.

- [stats](https://tidyclust.tidymodels.org/dev/reference/details_k_means_stats.md):
  Classical K-means

- [ClusterR](https://tidyclust.tidymodels.org/dev/reference/details_k_means_ClusterR.md):
  Classical K-means

- [klaR](https://tidyclust.tidymodels.org/dev/reference/details_k_means_klaR.md):
  K-Modes

- [clustMixType](https://tidyclust.tidymodels.org/dev/reference/details_k_means_clustMixType.md):
  K-prototypes

## Usage

``` r
k_means(mode = "partition", engine = "stats", num_clusters = NULL)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "partition".

- engine:

  A single character string specifying what computational engine to use
  for fitting. Possible engines are listed below. The default for this
  model is `"stats"`.

- num_clusters:

  Positive integer, number of clusters in model.

## Value

A `k_means` cluster specification.

## Details

### What does it mean to predict?

For a K-means model, each cluster is defined by a location in the
predictor space. Therefore, prediction in tidyclust is defined by
calculating which cluster centroid an observation is closest too.

## Examples

``` r
# Show all engines
modelenv::get_from_env("k_means")
#> # A tibble: 4 × 2
#>   engine       mode     
#>   <chr>        <chr>    
#> 1 stats        partition
#> 2 ClusterR     partition
#> 3 clustMixType partition
#> 4 klaR         partition

k_means()
#> K Means Cluster Specification (partition)
#> 
#> Computational engine: stats 
#> 
```
