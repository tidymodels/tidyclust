# K-means via clustMixType

[`k_means()`](https://tidyclust.tidymodels.org/dev/reference/k_means.md)
creates K-prototypes model. A K-prototypes is the middle ground between
a K-means and K-modes model, in the sense that it can be used with data
that contains both numeric and categorical predictors.

## Details

Both numeric and categorical predictors are requires for this engine.

For this engine, there is a single mode: partition

### Tuning Parameters

This model has 1 tuning parameters:

- `num_clusters`: \# Clusters (type: integer, default: no default)

### Translation from tidyclust to the original package (partition)

    k_means(num_clusters = integer(1)) |>
      set_engine("clustMixType") |>
      set_mode("partition") |>
      translate_tidyclust()

    ## K Means Cluster Specification (partition)
    ##
    ## Main Arguments:
    ##   num_clusters = integer(1)
    ##
    ## Computational engine: clustMixType
    ##
    ## Model fit template:
    ## tidyclust::.k_means_fit_clustMixType(x = missing_arg(), k = missing_arg(),
    ##     keep.data = missing_arg(), k = integer(1), keep.data = TRUE,
    ##     verbose = FALSE)

### Preprocessing requirements

Both categorical and numeric predictors are required.

### References

- Szepannek, G. (2018): clustMixType: User-Friendly Clustering of
  Mixed-Type Data in R, The R Journal 10/2, 200-208,
  <doi:10.32614/RJ-2018-048>.

- Aschenbruck, R., Szepannek, G., Wilhelm, A. (2022): Imputation
  Strategies for Clustering Mixed‑Type Data with Missing Values, Journal
  of Classification, <doi:10.1007/s00357-022-09422-y>.

- Z.Huang (1998): Extensions to the k-Means Algorithm for Clustering
  Large Data Sets with Categorical Variables, Data Mining and Knowledge
  Discovery 2, 283-304.
