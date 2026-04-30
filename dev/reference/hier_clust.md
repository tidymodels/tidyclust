# Hierarchical (Agglomerative) Clustering

`hier_clust()` defines a model that fits clusters based on a
distance-based dendrogram

There are different ways to fit this model, and the method of estimation
is chosen by setting the model engine. The engine-specific pages for
this model are listed below.

- [stats](https://tidyclust.tidymodels.org/dev/reference/details_hier_clust_stats.md)

## Usage

``` r
hier_clust(
  mode = "partition",
  engine = "stats",
  num_clusters = NULL,
  cut_height = NULL,
  linkage_method = "complete",
  dist_fun = NULL
)
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

  Positive integer, number of clusters in model (optional).

- cut_height:

  Positive double, height at which to cut dendrogram to obtain cluster
  assignments (only used if `num_clusters` is `NULL`)

- linkage_method:

  the agglomeration method to be used. This should be (an unambiguous
  abbreviation of) one of `"ward.D"`, `"ward.D2"`, `"single"`,
  `"complete"`, `"average"` (= UPGMA), `"mcquitty"` (= WPGMA),
  `"median"` (= WPGMC) or `"centroid"` (= UPGMC).

- dist_fun:

  A function for calculating the distance between observations. Defaults
  to
  [`philentropy::distance`](https://drostlab.github.io/philentropy/reference/distance.html)
  which supports numerous distance metrics via its `method` argument.
  The function should accept a matrix or data frame and return a square
  numeric matrix or an object coercible to one via
  [`stats::as.dist()`](https://rdrr.io/r/stats/dist.html). See
  [`silhouette()`](https://tidyclust.tidymodels.org/dev/reference/silhouette.md)
  for further details.

## Details

### What does it mean to predict?

To predict the cluster assignment for a new observation, we find the
closest cluster. How we measure “closeness” is dependent on the
specified type of linkage in the model:

- *single linkage*: The new observation is assigned to the same cluster
  as its nearest observation from the training data.

- *complete linkage*: The new observation is assigned to the cluster
  with the smallest maximum distances between training observations and
  the new observation.

- *average linkage*: The new observation is assigned to the cluster with
  the smallest average distances between training observations and the
  new observation.

- *centroid method*: The new observation is assigned to the cluster with
  the closest centroid, as in prediction for k_means.

## Examples

``` r
# Show all engines
modelenv::get_from_env("hier_clust")
#> # A tibble: 1 × 2
#>   engine mode     
#>   <chr>  <chr>    
#> 1 stats  partition

hier_clust()
#> Hierarchical Clustering Specification (partition)
#> 
#> Main Arguments:
#>   linkage_method = complete
#> 
#> Computational engine: stats 
#> 
```
