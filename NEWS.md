# tidyclust 0.1.2

* The cluster specification methods for `generics::tune_args()` and `generics::tunable()` are now registered unconditionally (#115).

# tidyclust 0.1.1

* Fixed bug where `extract_cluster_assignment()` and `predict()` sometimes didn't have agreement of clusters. (#94)

* `silhouette()` and `silhouette_avg()` now return NAs instead of erroring when applied to a clustering object with 1 cluster. (#104)

* Fixed bug where `extract_cluster_assignment()` doesn't work for `hier_clust()` models in workflows where `num_clusters` is specified in `extract_cluster_assignment()`.

# tidyclust 0.1.0

* Added a `NEWS.md` file to track changes to the package.
