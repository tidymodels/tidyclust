# extract_cluster_assignment() errors for cluster spec

    Code
      extract_cluster_assignment(spec)
    Condition
      Error in `extract_cluster_assignment()`:
      ! This function requires a fitted model.
      i Please use `fit()` on your cluster specification.

# extract_cluster_assignment() errors for hier_clust() with missing args

    Code
      extract_cluster_assignment(hclust_fit)
    Condition
      Error in `extract_cluster_assignment()`:
      ! Please specify either `num_clusters` or `cut_height`.

# extract_cluster_assignment() errors for hier_clust() with k arg

    Code
      extract_cluster_assignment(hclust_fit, k = 3)
    Condition
      Error in `extract_cluster_assignment()`:
      ! Using `k` argument is not supported.
      i Please use `num_clusters` instead.

# extract_cluster_assignment() errors for hier_clust() with h arg

    Code
      extract_cluster_assignment(hclust_fit, h = 3)
    Condition
      Error in `extract_cluster_assignment()`:
      ! Using `h` argument is not supported.
      i Please use `cut_height` instead.

# labels length mismatch errors in extract_cluster_assignment()

    Code
      extract_cluster_assignment(spec, labels = c("A", "B"))
    Condition
      Error in `make_cluster_labels()`:
      ! `labels` must have length 3, not 2.

# duplicate labels errors in extract_cluster_assignment()

    Code
      extract_cluster_assignment(spec, labels = c("A", "A", "B"))
    Condition
      Error in `make_cluster_labels()`:
      ! `labels` must not contain duplicate values. Duplicated: "A".

