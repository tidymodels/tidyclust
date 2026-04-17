# mclust fit helper function

This function returns the mclust model name based on the specified
TRUE/FALSE model arguments.

## Usage

``` r
mclust_helper(
  circular,
  zero_covariance,
  shared_orientation,
  shared_shape,
  shared_size
)
```

## Arguments

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

string containing mclust model name
