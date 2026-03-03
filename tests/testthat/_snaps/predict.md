# predict() errors for cluster spec

    Code
      predict(spec)
    Condition
      Error in `predict()`:
      ! This function requires a fitted model.
      i Please use `fit()` on your cluster specification.

# predict() errors for hier_clust() with missing args

    Code
      predict(hclust_fit, mtcars)
    Condition
      Error in `predict()`:
      ! Please specify either `num_clusters` or `cut_height`.

# predict() errors for hier_clust() with k arg

    Code
      predict(hclust_fit, mtcars, k = 3)
    Condition
      Error in `predict()`:
      ! Using `k` argument is not supported.
      i Please use `num_clusters` instead.

# predict() errors for hier_clust() with h arg

    Code
      predict(hclust_fit, mtcars, h = 3)
    Condition
      Error in `predict()`:
      ! Using `h` argument is not supported.
      i Please use `cut_height` instead.

# predict() errors for cluster spec for freq_itemsets

    Code
      predict(spec)
    Condition
      Error in `predict()`:
      ! This function requires a fitted model.
      i Please use `fit()` on your cluster specification.

