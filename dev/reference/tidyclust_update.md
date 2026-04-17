# Update a cluster specification

If parameters of a cluster specification need to be modified,
[`update()`](https://rdrr.io/r/stats/update.html) can be used in lieu of
recreating the object from scratch.

## Usage

``` r
# S3 method for class 'db_clust'
update(
  object,
  parameters = NULL,
  radius = NULL,
  min_points = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'gm_clust'
update(
  object,
  parameters = NULL,
  num_clusters = NULL,
  circular = NULL,
  zero_covariance = NULL,
  shared_orientation = NULL,
  shared_shape = NULL,
  shared_size = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'hier_clust'
update(
  object,
  parameters = NULL,
  num_clusters = NULL,
  cut_height = NULL,
  linkage_method = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'k_means'
update(object, parameters = NULL, num_clusters = NULL, fresh = FALSE, ...)
```

## Arguments

- object:

  A cluster specification.

- parameters:

  A 1-row tibble or named list with *main* parameters to update. Use
  **either** `parameters` **or** the main arguments directly when
  updating. If the main arguments are used, these will supersede the
  values in `parameters`. Also, using engine arguments in this object
  will result in an error.

- radius:

  Positive double, Radius drawn around points to determine core-points
  and cluster assignments (required).

- min_points:

  Positive integer, Minimum number of connected points required to form
  a core-point, including the point itself (required).

- fresh:

  A logical for whether the arguments should be modified in-place or
  replaced wholesale.

- ...:

  Not used for [`update()`](https://rdrr.io/r/stats/update.html).

- num_clusters:

  Positive integer, number of clusters in model.

- circular:

  Boolean, whether or not to fit circular MVG distributions for each
  cluster. Default `TRUE`.

- zero_covariance:

  Boolean, whether or not to assign covariances of 0 for each MVG.
  Default `TRUE`.

- shared_orientation:

  Boolean, whether each cluster MVG should have the same orientation.
  Default `TRUE`.

- shared_shape:

  Boolean, whether each cluster MVG should have the same shape. Default
  `TRUE`.

- shared_size:

  Boolean, whether each cluster MVG should have the same size/volume.
  Default `TRUE`.

- cut_height:

  Positive double, height at which to cut dendrogram to obtain cluster
  assignments (only used if `num_clusters` is `NULL`)

- linkage_method:

  the agglomeration method to be used. This should be (an unambiguous
  abbreviation of) one of `"ward.D"`, `"ward.D2"`, `"single"`,
  `"complete"`, `"average"` (= UPGMA), `"mcquitty"` (= WPGMA),
  `"median"` (= WPGMC) or `"centroid"` (= UPGMC).

## Value

An updated cluster specification.

## Examples

``` r
kmeans_spec <- k_means(num_clusters = 5)
kmeans_spec
#> K Means Cluster Specification (partition)
#> 
#> Main Arguments:
#>   num_clusters = 5
#> 
#> Computational engine: stats 
#> 
update(kmeans_spec, num_clusters = 1)
#> K Means Cluster Specification (partition)
#> 
#> Main Arguments:
#>   num_clusters = 1
#> 
#> Computational engine: stats 
#> 
update(kmeans_spec, num_clusters = 1, fresh = TRUE)
#> K Means Cluster Specification (partition)
#> 
#> Main Arguments:
#>   num_clusters = 1
#> 
#> Computational engine: stats 
#> 

param_values <- tibble::tibble(num_clusters = 10)

kmeans_spec |> update(param_values)
#> K Means Cluster Specification (partition)
#> 
#> Main Arguments:
#>   num_clusters = 10
#> 
#> Computational engine: stats 
#> 
```
