# tidyclust (development version)

* Fixed bug where engine specific arguments were passed along for `k_means()` when the engine ClusterR. (#142)

* Fixed bug where `prefix` argument wouldn't be correctly passed through `extract_cluster_assignment()`, `extract_centroids()`, and `predict()` (#145)

* Metric functions now error informatively if used with unfit cluster specifications. (#146)

* Using `extract_cluster_assignment()`, `extract_centroids()` and `predict()` on a fitted `hier_clust()` model without specifying `num_clust` or `cut_height` now gives more informative error message. (#147)

* `k_means()` now errors informatively if `fit()` without `num_clust` specified. (#134)

# tidyclust 0.1.2

* The cluster specification methods for `generics::tune_args()` and `generics::tunable()` are now registered unconditionally (#115).

# tidyclust 0.1.1

* Fixed bug where `extract_cluster_assignment()` and `predict()` sometimes didn't have agreement of clusters. (#94)

* `silhouette()` and `silhouette_avg()` now return NAs instead of erroring when applied to a clustering object with 1 cluster. (#104)

* Fixed bug where `extract_cluster_assignment()` doesn't work for `hier_clust()` models in workflows where `num_clusters` is specified in `extract_cluster_assignment()`.

# tidyclust 0.1.0

* Added a `NEWS.md` file to track changes to the package.
