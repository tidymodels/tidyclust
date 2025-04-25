# extract_centroids() errors for cluster spec

    Code
      extract_centroids(spec)
    Condition
      Error in `extract_centroids()`:
      ! This function requires a fitted model.
      i Please use `fit()` on your cluster specification.

# extract_centroids() errors for hier_clust() with missing args

    Code
      extract_centroids(hclust_fit)
    Condition
      Error in `extract_centroids()`:
      ! Please specify either `num_clusters` or `cut_height`.

# extract_centroids() errors for hier_clust() with k arg

    Code
      extract_centroids(hclust_fit, k = 3)
    Condition
      Error in `extract_centroids()`:
      ! Using `k` argument is not supported.
      i Please use `num_clusters` instead.

# extract_centroids() errors for hier_clust() with h arg

    Code
      extract_centroids(hclust_fit, h = 3)
    Condition
      Error in `extract_centroids()`:
      ! Using `h` argument is not supported.
      i Please use `cut_height` instead.

