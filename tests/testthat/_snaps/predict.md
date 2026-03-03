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

# predict with type = 'raw' errors when not available

    Code
      predict(fit, mtcars, type = "raw")
    Condition
      Error in `check_spec_pred_type()`:
      ! No raw prediction method available for this model.
      i `type` should be one of "cluster".

# predict errors with NA in new_data

    Code
      predict(fit, new_data)
    Condition
      Error in `flexclust::dist2()`:
      ! Cannot handle missing values!

# predict errors with missing required columns

    Code
      predict(fit, missing_cols)
    Condition
      Error:
      ! object 'wt' not found

