# tidyclust (development version)

# tidyclust 0.2.4

* The philentropy package is now used to calculate distances rather than Rfast. (#199)

# tidyclust 0.2.3

* Update to fix revdep issue for clustMixType. (#190)

# tidyclust 0.2.2

* Update to fix revdep issue for ClusterR. (#186)

# tidyclust 0.2.1

* Small change to let tune package have easy CRAN release. (#178)

# tidyclust 0.2.0

## New Engines

* The clustMixType engine as been added to `k_means()`. This engine allows fitting of k-prototype models. (#63)

* The klaR engine as been added to `k_means()`. This engine allows fitting of k-modes models. (#63)

## Improvements

* Engine specific documentation has been added for all models and engines. (#159)

## Bug Fixes

* Fixed bug where engine specific arguments were passed along for `k_means()` when the engine ClusterR. (#142)

* Fixed bug where `prefix` argument wouldn't be correctly passed through `extract_cluster_assignment()`, `extract_centroids()`, and `predict()` (#145)

* Metric functions now error informatively if used with unfit cluster specifications. (#146)

* Fixed bug that caused cluster ordering in extract_fit_summary(). (#136)

* Using `extract_cluster_assignment()`, `extract_centroids()` and `predict()` on a fitted `hier_clust()` model without specifying `num_clust` or `cut_height` now gives more informative error message. (#147)

* `k_means()` now errors informatively if `fit()` without `num_clust` specified. (#134)

* Fixed bug where levels didn't match number of clusters if prediction on fewer number of observations. (#158)

* Fixed bug where `tune_cluster()` would error if used with an recipe that contained non-predictor variables such as id variables. (#124)

## Breaking Changes

* Exported internal functions `ClusterR_kmeans_fit()`, `stats_kmeans_fit()`, and `hclust_fit()` have been renamed to `.k_means_fit_ClusterR()`, `.k_means_fit_stats()`, and `.hier_clust_fit_stats()` to reduce visibility for users. 

* Cluster reordering is now done at the fitting time, not the extraction and prediction time. (#154)

# tidyclust 0.1.2

* The cluster specification methods for `generics::tune_args()` and `generics::tunable()` are now registered unconditionally (#115).

# tidyclust 0.1.1

* Fixed bug where `extract_cluster_assignment()` and `predict()` sometimes didn't have agreement of clusters. (#94)

* `silhouette()` and `silhouette_avg()` now return NAs instead of erroring when applied to a clustering object with 1 cluster. (#104)

* Fixed bug where `extract_cluster_assignment()` doesn't work for `hier_clust()` models in workflows where `num_clusters` is specified in `extract_cluster_assignment()`.

# tidyclust 0.1.0

* Added a `NEWS.md` file to track changes to the package.
