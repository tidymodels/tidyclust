# Update a cluster specification

If parameters of a cluster specification need to be modified,
[`update()`](https://rdrr.io/r/stats/update.html) can be used in lieu of
recreating the object from scratch.

## Usage

``` r
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

- num_clusters:

  Positive integer, number of clusters in model.

- cut_height:

  Positive double, height at which to cut dendrogram to obtain cluster
  assignments (only used if `num_clusters` is `NULL`)

- linkage_method:

  the agglomeration method to be used. This should be (an unambiguous
  abbreviation of) one of `"ward.D"`, `"ward.D2"`, `"single"`,
  `"complete"`, `"average"` (= UPGMA), `"mcquitty"` (= WPGMA),
  `"median"` (= WPGMC) or `"centroid"` (= UPGMC).

- fresh:

  A logical for whether the arguments should be modified in-place or
  replaced wholesale.

- ...:

  Not used for [`update()`](https://rdrr.io/r/stats/update.html).

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
