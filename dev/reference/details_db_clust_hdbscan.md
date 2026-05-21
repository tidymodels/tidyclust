# Hierarchical Density-Based Spatial Clustering (HDBSCAN) via dbscan

[`db_clust()`](https://tidyclust.tidymodels.org/dev/reference/db_clust.md)
creates an HDBSCAN model.

## Details

For this engine, there is a single mode: partition

### Tuning Parameters

This model has 1 tuning parameters:

- `min_points`: Minimum Number of Points (type: integer, default: no
  default)

The `hdbscan` engine also accepts the engine-specific argument
`min_cluster_size` (passed via
`set_engine("hdbscan", min_cluster_size = ...)`). When supplied, it
overrides `min_points` as the value of `minPts` passed to
[`dbscan::hdbscan()`](https://rdrr.io/pkg/dbscan/man/hdbscan.html). If
not supplied, `min_points` is used.

### Translation from tidyclust to the original package (partition)

    db_clust(min_points = 5) |>
      set_engine("hdbscan") |>
      set_mode("partition") |>
      translate_tidyclust()

    ## DBSCAN Clustering Specification (partition)
    ##
    ## Main Arguments:
    ##   min_points = 5
    ##
    ## Computational engine: hdbscan
    ##
    ## Model fit template:
    ## tidyclust::.db_clust_fit_hdbscan(x = missing_arg(), min_points = missing_arg(),
    ##     min_points = 5)

### Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://tidyclust.tidymodels.org/dev/reference/fit.md),
tidyclust will convert factor columns to indicators.

Predictors should have the same scale. One way to achieve this is to
center and scale each so that each predictor has mean zero and a
variance of one.

### What does it mean to predict?

To predict the cluster assignment for a new observation, the nearest
training observation that was not classified as noise is found. The new
observation is assigned to that neighbor’s cluster if the distance is at
most the neighbor’s core distance; otherwise the new observation is
marked as an outlier.

### References

- Campello, R. J. G. B., Moulavi, D., & Sander, J. (2013). Density-Based
  Clustering Based on Hierarchical Density Estimates. In *Advances in
  Knowledge Discovery and Data Mining* (Vol. 7819, pp. 160–172).
  Springer.
  <https://link.springer.com/chapter/10.1007/978-3-642-37456-2_14>

- Campello, R. J. G. B., Moulavi, D., Zimek, A., & Sander, J. (2015).
  Hierarchical density estimates for data clustering, visualization, and
  outlier detection. *ACM Transactions on Knowledge Discovery from
  Data*, 10(1), 1–51. <https://dl.acm.org/doi/10.1145/2733381>

- Hahsler, M., Piekenbrock, M., & Doran, D. (2019). dbscan: Fast
  Density-Based Clustering with R. *Journal of Statistical Software*,
  91(1). <https://www.jstatsoft.org/article/view/v091i01>
