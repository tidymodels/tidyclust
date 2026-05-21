# Simple Wrapper around ClusterR kmeans

This wrapper runs
[`ClusterR::KMeans_rcpp()`](https://mlampros.github.io/ClusterR/reference/KMeans_rcpp.html)
and adds column names to the `centroids` field. And reorders the
clusters.

## Usage

``` r
.k_means_fit_ClusterR(
  data,
  clusters,
  num_init = 1,
  max_iters = 100,
  initializer = "kmeans++",
  fuzzy = FALSE,
  verbose = FALSE,
  CENTROIDS = NULL,
  tol = 1e-04,
  tol_optimal_init = 0.3,
  seed = 1
)
```

## Arguments

- data:

  matrix or data frame

- clusters:

  the number of clusters

- num_init:

  number of times the algorithm will be run with different centroid
  seeds

- max_iters:

  the maximum number of clustering iterations

- initializer:

  the method of initialization. One of, optimal_init, quantile_init,
  kmeans++ and random. See details for more information

- fuzzy:

  either TRUE or FALSE. If TRUE, then prediction probabilities will be
  calculated using the distance between observations and centroids

- verbose:

  either TRUE or FALSE, indicating whether progress is printed during
  clustering.

- CENTROIDS:

  a matrix of initial cluster centroids. The rows of the CENTROIDS
  matrix should be equal to the number of clusters and the columns
  should be equal to the columns of the data.

- tol:

  a float number. If, in case of an iteration (iteration \> 1 and
  iteration \< max_iters) 'tol' is greater than the squared norm of the
  centroids, then kmeans has converged

- tol_optimal_init:

  tolerance value for the 'optimal_init' initializer. The higher this
  value is, the far appart from each other the centroids are.

- seed:

  integer value for random number generator (RNG)

## Value

a list with the following attributes: clusters, fuzzy_clusters (if fuzzy
= TRUE), centroids, total_SSE, best_initialization, WCSS_per_cluster,
obs_per_cluster, between.SS_DIV_total.SS
