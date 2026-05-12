# Simple Wrapper around hdbscan function

This wrapper passes the data to
[`dbscan::hdbscan()`](https://rdrr.io/pkg/dbscan/man/hdbscan.html) and
stashes the training data on the result so it can be reused for
prediction and extraction.

## Usage

``` r
.db_clust_fit_hdbscan(x, min_points = NULL, min_cluster_size = NULL, ...)
```

## Arguments

- x:

  matrix or data frame.

- min_points:

  Minimum cluster size used as the `minPts` argument of
  [`dbscan::hdbscan()`](https://rdrr.io/pkg/dbscan/man/hdbscan.html).

- min_cluster_size:

  Engine-specific override for `minPts`. When supplied, it is used in
  place of `min_points`.

## Value

hdbscan object
