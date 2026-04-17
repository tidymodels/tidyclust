# Simple Wrapper around Mclust function

This wrapper prepares the data into a distance matrix to send to
[`mclust::Mclust`](https://mclust-org.github.io/mclust/reference/Mclust.html)
and retains the parameters `num_clusters` as an attribute.

## Usage

``` r
.gm_clust_fit_mclust(
  x,
  num_clusters = NULL,
  circular = NULL,
  zero_covariance = NULL,
  shared_orientation = NULL,
  shared_shape = NULL,
  shared_size = NULL,
  ...
)
```

## Arguments

- x:

  matrix or data frame.

- num_clusters:

  Number of clusters.

- circular:

  Whether or not to fit circular MVG distributions for each cluster.

- zero_covariance:

  Whether or not to assign covariances of 0 for each MVG.

- shared_orientation:

  Whether each cluster MVG should have the same orientation.

- shared_shape:

  Whether each cluster MVG should have the same shape.

- shared_size:

  Whether each cluster MVG should have the same size/volume.

## Value

mclust object
