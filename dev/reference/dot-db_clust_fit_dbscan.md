# Simple Wrapper around dbscan function

This wrapper prepares the data into a distance matrix to send to
[`dbscan::dbscan()`](https://rdrr.io/pkg/dbscan/man/dbscan.html) and
retains the parameters `radius` or `min_points` as an attribute.

## Usage

``` r
.db_clust_fit_dbscan(x, radius = NULL, min_points = NULL, ...)
```

## Arguments

- x:

  matrix or data frame.

- radius:

  Radius used to determine core-points and cluster points together.

- min_points:

  Minimum number of points needed to form a cluster.

## Value

dbscan object
