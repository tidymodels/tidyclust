# extract_cluster_assignment() errors for cluster spec

    Code
      extract_cluster_assignment(spec)
    Condition
      Error in `extract_cluster_assignment()`:
      ! This function requires a fitted model. Please use `fit()` on your cluster specification.

# extract_cluster_assignment() errors for hier_clust() with missing args

    Code
      hclust_fit %>% extract_cluster_assignment()
    Condition
      Error in `extract_cluster_assignment()`:
      ! Please specify either `num_clusters` or `cut_height`.

# extract_cluster_assignment() errors for hier_clust() with k arg

    Code
      hclust_fit %>% extract_cluster_assignment(k = 3)
    Condition
      Error in `extract_cluster_assignment()`:
      ! Using `k` argument is not supported. Please use `num_clusters` instead.

# extract_cluster_assignment() errors for hier_clust() with h arg

    Code
      hclust_fit %>% extract_cluster_assignment(h = 3)
    Condition
      Error in `extract_cluster_assignment()`:
      ! Using `h` argument is not supported. Please use `cut_height` instead.

